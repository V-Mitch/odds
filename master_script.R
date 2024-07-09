# master script for project execution (temporary)
# setwd("C:/Users/victo/OneDrive/Spectre/R tests/Betting")
source("repo_class.R")
source("DataHandlers.R")
source("Calculations.R")
source("football_Repo_2.R")
source("api.R")
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



# load("2024-06-27__avg_df")
load.Rdata("2024-06-28__2020_leagues_friendlies", "df1")
load.Rdata("2024-06-27__2021_leagues_friendlies", "df2")
load.Rdata("2024-06-27__2022_leagues_friendlies", "df3")
load.Rdata("2024-06-27__2023_leagues_friendlies", "df4")
load.Rdata("2024-06-27__2024_leagues_friendlies", "df5")
#(new)
load.Rdata("2024-07-05__2019_leagues_friendlies", "df6")
load.Rdata("2024-07-05__2018_leagues_friendlies", "df7")
load.Rdata("2024-07-05__2017_leagues_friendlies", "df8")
#
load.Rdata("2024-07-04__2022_1", "df9")
load.Rdata("2024-07-05__2018_1", "df10")
load.Rdata("2024-07-04__2020_5", "df11")
load.Rdata("2024-07-04__2022_5", "df12")
load.Rdata("2024-07-04__2024_5", "df13")
load.Rdata("2024-07-04__2020_4", "df14")
load.Rdata("2024-07-04__2024_4", "df15")
load.Rdata("2024-07-04__2023_960", "df16")
#(new)
load.Rdata("2024-07-05__2023_22", "df17")
load.Rdata("2024-07-05__2021_22", "df18")
load.Rdata("2024-07-05__2019_22", "df19")
load.Rdata("2024-07-05__2017_22", "df20")
load.Rdata("2024-07-05__2015_22", "df21")
#(new)
load.Rdata("2024-07-05__2018_29", "df22")
load.Rdata("2024-07-05__2022_29", "df23")
load.Rdata("2024-07-05__2023_29", "df24")
#(new)
load.Rdata("2024-07-05__2022_30", "df25")
load.Rdata("2024-07-05__2018_30", "df26")
#(new)
load.Rdata("2024-07-05__2022_31", "df27")
load.Rdata("2024-07-05__2018_31", "df28")
#(new)
load.Rdata("2024-07-05__2020_32", "df29")
load.Rdata("2024-07-05__2018_32", "df30")
#(new)
load.Rdata("2024-07-05__2022_34", "df31")
load.Rdata("2024-07-05__2018_34", "df32")
#(new)
load.Rdata("2024-07-05__2022_35", "df33")
load.Rdata("2024-07-05__2019_35", "df34")
# (new)
load.Rdata("2024-07-05__2022_37", "df35")
load.Rdata("2024-07-05__2018_37", "df36")
load.Rdata("2024-07-05__2014_37", "df37")
# (new)
# load.Rdata("2024-07-05__2023_38", "df38")
# load.Rdata("2024-07-05__2021_38", "df39")
# load.Rdata("2024-07-05__2017_38", "df40")
df_list <- mget(paste0("df", 1:37))
# Combine the different years together from the raw datasets
# fixtures_training <- bind_rows(fixtures_training_2020, fixtures_training_2021, fixtures_training_2022, 
#                                fixtures_training_2023, fixtures_training_2024) %>% 
fixtures_training <- bind_rows(df_list)

# convert some to numeric for better handling
fixtures_training <- fixtures_training %>% 
  mutate(across(any_of(post_game_varnames), as.numeric))

# FEATURE ENGINEERING
#Functions

fixtures_training <- fixtures_training %>% arrange(desc(fixture.date))

avg_df <- calculate_averages(fixtures_training, post_game_varnames, 
                             fixture_lookback = 20,
                             weighting = "soft")

avg_df <- as.data.frame(lapply(avg_df, function(x) sapply(x, unlist)))

fixtures_training <- calculate_time_diff(fixtures_training)
# games_played_recent <- 
namefile <- paste0(Sys.Date(), "__", "avg_df_9thjul")
save(avg_df, file = namefile)
# load(namefile)
# setting the output variable
target_variable <- ifelse(is.na(fixtures_training$teams.home.winner), "draw",
                          ifelse(fixtures_training$teams.home.winner == 1, "home",
                                 ifelse(fixtures_training$teams.home.winner == 0, "away", NA)))
fixtures_training$outcome <- target_variable
fixtures_training$outcome <- as.factor(target_variable)
# de-select forward-looking bias variables, and add aggregated post_games
fixtures_training <- fixtures_training[!duplicated(fixtures_training),]
avg_df <- avg_df[!duplicated(avg_df$fixture.id),]
full_set <- left_join(
  fixtures_training[, !names(fixtures_training) %in% post_game_varnames], avg_df,
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

character_cols <- sapply(clean_set, is.character)
clean_set[, character_cols][is.na(clean_set[, character_cols])] <- "no-value"
clean_set[is.na(clean_set)] <- -1
# only want finished games included - forward-looking variable
# 
target_games <- clean_set[clean_set$fixture.id %in% c(1219688, 1219959, 1219689, 1220308) ,]
target_games <- target_games %>% select(-c(fixture.status.long, fixture.status.elapsed))

clean_set <- clean_set[clean_set$fixture.status.long == "Match Finished",]
clean_set <- clean_set %>% select(-c(fixture.status.long, fixture.status.elapsed))

# Calculate class frequencies
class_counts <- table(train_set$outcome)
total_samples <- sum(class_counts)
# Calculate class weights (inverse frequency)
class_weights <- total_samples / (length(class_counts) * class_counts)
# Assign weights to each instance in the training set
instance_weights <- sapply(train_set$outcome, function(x) class_weights[x])

# REGRESSION TREE
save.image(file = paste0(Sys.Date(),"ready_for_execution_5_eve.RData"))
# load("2024-06-30ready_for_execution.RData")
forest_spec <- rand_forest(trees = 10000, 
                           min_n = 1, 
                           mtry = round(sqrt(ncol(clean_set))), 
                           mode = "classification") %>% 
  set_engine("ranger", importance = "impurity", case.weights = instance_weights)
             # 
split_set <- initial_split(clean_set, prop = 0.75, strata = outcome)
train_set <- training(split_set)
test_set <- testing(split_set)

# train_set_balanced <- upSample(x = train_set[, -which(names(train_set) == "outcome")], 
#                                y = train_set$outcome)
# train_set_balanced <- bind_cols(train_set_balanced, outcome = train_set_balanced$Class)
# train_set_balanced$Class <- NULL


x_vars <- train_set %>%
  select(-outcome)
y_var <- train_set %>%
  select(outcome)

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


