# master script for project execution (temporary)
# setwd("C:/Users/victo/OneDrive/Spectre/R tests/Betting")
source("repo_class.R")
source("DataHandlers.R")
source("Calculations.R")
source("football_Repo_2.R")
source("api.R")
source("Manual_data_loads.R")
library(tidymodels)
library(ranger)
library(randomForest)
library(rpart.plot)
library(caret)
library(ggplot2)
library(reshape2)  
library(vip)
library(pdp)
library(miceadds)

sports_repo = new("Sports_Repository")
sports_repo = updateSportsFromAPI(sports_repo, all = "false")
sports_repo@data$title

events_repo = new("Events_Repository")
events_repo = updateEventsFromAPI(events_repo, 
                                  sport = "soccer_uefa_european_championship")


odds_repo = new("Odds_Repository")
odds_repo = updateOddsFromAPI(odds_repo, 
                              sport = "soccer_uefa_european_championship", 
                              market = "h2h", regions = "uk,eu")

odds_table = extract_over_row(odds_repo@data)
odds_table[odds_table["key"] == "betfair_ex_eu",]

teams_repo = new("Teams_Repository")
teams_repo = updateTeamsFromAPI(teams_repo, competition = "EC")


teams_euro2024 <- c("Germany", "Scotland", "Hungary", "Switzerland", "Spain", 
                    "Croatia", "Italy", "Albania", "Slovenia", "Denmark", 
                    "Serbia", "England", "Poland", "Netherlands", "Austria",
                    "France", "Belgium", "Slovakia", "Romania", "Ukraine",
                    "Turkey", "Georgia", "Portugal", "Czech Republic")





fixtures_training <- bind_rows(df_list)
# convert some to numeric for better handling
fixtures_training <- fixtures_training %>% 
  mutate(across(any_of(post_game_varnames), as.numeric))

fixtures_training <- fixtures_training %>% arrange(desc(fixture.date))
fixtures_training <- fixtures_training %>% 
  mutate(fixture.date = as.Date(fixture.date)) %>% 
  mutate(teams.home.name = case_when(
    teams.home.name == "Korea Republic" ~ "South Korea",
    TRUE ~ teams.home.name  # keep other country names unchanged
  )) %>% 
  mutate(teams.away.name = case_when(
    teams.away.name == "Korea Republic" ~ "South Korea",
    TRUE ~ teams.away.name  # keep other country names unchanged
  ))

# Step 2: Join for home teams
home_join <- fixtures_training %>%
  left_join(fifa_rank, by = c("teams.home.name" = "country_full")) %>%
  filter(rank_date <= fixture.date) %>%
  group_by(teams.home.name, fixture.date) %>%
  filter(rank_date == max(rank_date)) %>%
  rename(
    confederation.home = confederation , 
    rank.date.home = rank_date, 
    rank.home = rank, 
    total.points.home = total_points,
    previous.points.home = previous_points,
    rank.change.home = rank_change
  ) %>%
  ungroup()

# Step 3: Join for away teams
away_join <- fixtures_training %>%
  left_join(fifa_rank, by = c("teams.away.name" = "country_full")) %>%
  filter(rank_date <= fixture.date) %>%
  group_by(teams.away.name, fixture.date) %>%
  filter(rank_date == max(rank_date)) %>%
  rename(
    confederation.away = confederation, 
    rank.date.away = rank_date, 
    rank.away = rank, 
    total.points.away = total_points,
    previous.points.away = previous_points,
    rank.change.away = rank_change) %>% 
  ungroup()

# Step 4: Combine the results
final_data <- home_join %>%
  left_join(select(away_join, teams.away.name, confederation.away, 
                   rank.date.away, rank.away, 
                   total.points.away, previous.points.away, rank.change.away,
                   fixture.date, fixture.id), 
            by = c("teams.away.name", "fixture.date", "fixture.id"))
final_data <- final_data %>% as_tibble()
final_data <- final_data %>% 
  mutate(across(any_of(post_game_varnames), as.numeric))
final_data <- final_data %>% arrange(desc(fixture.date))

avg_df <- calculate_averages(final_data, post_game_varnames, 
                             fixture_lookback = 20,
                             weighting = "soft")


final_data <- calculate_time_diff(final_data)
# games_played_recent <- 
namefile <- paste0(Sys.Date(), "__", "avg_df_10thjul")
save(final_data, file = namefile)
# load(namefile)
# setting the output variable
target_variable <- ifelse(is.na(final_data$teams.home.winner), "draw",
                          ifelse(final_data$teams.home.winner == 1, "home",
                                 ifelse(final_data$teams.home.winner == 0, "away", NA)))
final_data$outcome <- target_variable
final_data$outcome <- as.factor(target_variable)
# de-select forward-looking bias variables, and add aggregated post_games
final_data <- final_data[!duplicated(final_data),]
avg_df <- avg_df[!duplicated(avg_df$fixture.id),]
full_set <- left_join(
  final_data[, !names(final_data) %in% post_game_varnames], avg_df,
                      by = "fixture.id") %>% 
  select(-any_of(redundant_vars)) %>% 
  suppressMessages()


# CLEANING

# Filter for missing values and correct match status
# remove full NaN column
clean_set <- full_set[, colSums(is.na(full_set)) < nrow(full_set)]
# remove dirty rows with very little data
clean_set <- clean_set[rowSums(is.na(clean_set)) <= 164, ]
# clean_set %>%
#   summarise(across(everything(), ~ sum(is.na(.))))
# impute the missing averages with a median
clean_set <- clean_set %>% 
  mutate(across(where(is.numeric), ~replace_na(., median(., na.rm=TRUE))))
clean_set <- clean_set %>% 
  mutate(rank.date.home = if_else(is.na(rank.date.home), fixture.date, rank.date.home)) %>% 
  mutate(rank.date.away = if_else(is.na(rank.date.away), fixture.date, rank.date.away))

character_cols <- sapply(clean_set, is.character)
clean_set[, character_cols][is.na(clean_set[, character_cols])] <- "no-value"
clean_set[is.na(clean_set)] <- -1
# only want finished games included - forward-looking variable
# 
target_games <- clean_set[clean_set$fixture.id %in% c(1227539, 1225853) ,]
target_games <- target_games %>% select(-c(fixture.status.long, fixture.status.elapsed))

clean_set <- clean_set[clean_set$fixture.status.long == "Match Finished",]
clean_set <- clean_set %>% select(-c(fixture.status.long, fixture.status.elapsed))
# clean_set <- clean_set[c(1:17, 148:212)]

# REGRESSION TREE
# train_set_balanced <- upSample(x = train_set[, -which(names(train_set) == "outcome")], 
#                                y = train_set$outcome)
# train_set_balanced <- bind_cols(train_set_balanced, outcome = train_set_balanced$Class)
# train_set_balanced$Class <- NULL

split_set <- initial_split(clean_set, prop = 0.75, strata = outcome)
train_set <- training(split_set)
test_set <- testing(split_set)

x_vars <- train_set %>%
  select(-outcome)
y_var <- train_set %>%
  select(outcome)

# Calculate class frequencies
class_counts <- table(train_set$outcome)
total_samples <- sum(class_counts)
# Calculate class weights (inverse frequency)
class_weights <- total_samples / (length(class_counts) * class_counts)
# Assign weights to each instance in the training set
instance_weights <- sapply(train_set$outcome, function(x) class_weights[x])

# save.image(file = paste0(Sys.Date(),"ready_for_execution_10_eve.RData"))
# load("2024-06-30ready_for_execution.RData")
forest_spec <- rand_forest(trees = 10000, 
                           min_n = 1, 
                           mtry = round(sqrt(ncol(clean_set))), 
                           mode = "classification") %>% 
  set_engine("ranger", importance = "impurity", case.weights = instance_weights)
# 


# x_vars <- train_set_balanced %>%
#   select(-outcome)
# y_var <- train_set_balanced %>%
#   select(outcome)

model <- forest_spec %>% 
  fit_xy(x = x_vars , y = y_var, data = train_set)

predictions_prob <- predict(model, new_data = test_set, type = "prob")
predictions <- predict(model, new_data = test_set)

predictions_recent_prob <- predict(model, new_data = target_games, type = "prob")
predictions_recent <- predict(model, new_data = target_games)

# Create confusion matrix
cm <- confusionMatrix(predictions$.pred_class, test_set$outcome)
sum(cm$byClass[,"F1"] * cm$byClass[,"Prevalence"])


# INTERPRET
tree_model <- extract_fit_engine(model)
vip(tree_model, num_features = ncol(clean_set)-1)











# BOOSTED TREES model
rec <- recipe(outcome ~ . , data = train_set) %>% 
  step_zv () %>% 
  step_normalize(all_numeric(), -all_outcomes()) %>% 
  step_dummy(all_nominal(), -all_outcomes())
processed_training_data <- prep(rec) %>% juice()
processed_testing_data <- bake(prep(rec), new_data = test_set)


xgb_spec <- boost_tree(
  trees = 10000,  # Number of trees
  min_n = 1,  # Minimum number of data points in a node
  # tree_depth = 6,  # Depth of the tree
  # learn_rate = 0.01,  # Learning rate
  # loss_reduction = 0.01,  # Minimum loss reduction required for further partitioning
  # sample_size = 1,  # Fraction of observations to sample
  mtry = round(sqrt(ncol(clean_set))),  # Number of variables available for splitting at each tree node
  mode = "classification"  # Classification mode
) %>% 
  set_engine("xgboost", importance = "gain", case.weights = instance_weights)

x_vars <- processed_training_data %>%
  select(-outcome)
y_var <- processed_training_data %>%
  select(outcome)

model <- xgb_spec %>% 
  fit_xy(x = x_vars , y = y_var, data = processed_training_data)

predictions <- predict(model, new_data = processed_testing_data %>% select(-outcome))

# Create confusion matrix
cm <- confusionMatrix(predictions$.pred_class, processed_testing_data$outcome)
sum(cm$byClass[,"F1"] * cm$byClass[,"Prevalence"])

# INTERPRET
tree_model <- extract_fit_engine(model)
vip(tree_model, num_features = ncol(clean_set)-1)
