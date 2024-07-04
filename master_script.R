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

setwd("C:/Users/victo/OneDrive/Spectre/R tests/Betting")

# test 3
sports_repo = new("Sports_Repository")
sports_repo = updateSportsFromAPI(sports_repo, all = "false")
sports_repo@data$title

events_repo = new("Events_Repository")
events_repo = updateEventsFromAPI(events_repo, sport = "soccer_uefa_european_championship")


odds_repo = new("Odds_Repository")
odds_repo = updateOddsFromAPI(odds_repo, sport = "soccer_uefa_european_championship", market = "h2h", regions = "uk,eu")

odds_table = extract_over_row(odds_repo@data)
odds_table[odds_table["key"] == "betfair_ex_eu",]

teams_repo = new("Teams_Repository")
teams_repo = updateTeamsFromAPI(teams_repo, competition = "EC")


teams_euro2024 <- c("Germany", "Scotland", "Hungary", "Switzerland", "Spain", 
                    "Croatia", "Italy", "Albania", "Slovenia", "Denmark", 
                    "Serbia", "England", "Poland", "Netherlands", "Austria",
                    "France", "Belgium", "Slovakia", "Romania", "Ukraine",
                    "Turkey", "Georgia", "Portugal", "Czech Republic")



load("2024-06-27__avg_df")
x1 <- load_and_assign("2024-06-28__2020_leagues_friendlies", "x1")
x2 <- load("2024-06-27__2021_leagues_friendlies")
x3 <- load("2024-06-27__2022_leagues_friendlies")
x4 <- load("2024-06-27__2023_leagues_friendlies")
x5 <- load("2024-06-27__2024_leagues_friendlies")
x6 <- load("2024-06-27__2024_leagues_friendlies")
x7 <- load("2024-07-04__2022_1")
# x8 <- load("2024-07-04__2018_1")
x9 <- load("2024-07-04__2020_5")
x10 <- load("2024-07-04__2022_5")
x11 <- load("2024-07-04__2024_5")
x12 <- load("2024-07-04__2020_4")
x13 <- load("2024-07-04__2024_4")
x14 <- load("2024-07-04__2023_960")

# Combine the different years together from the raw datasets
fixtures_training <- bind_rows(fixtures_training_2020, fixtures_training_2021, fixtures_training_2022, 
                               fixtures_training_2023, fixtures_training_2024) %>% 
  fixtures_training <- bind_rows(x1, x2, x3, x4, x5, x6, x7, x9, x10, x11, x12, x13, x14)
  suppressMessages()

# convert some to numeric for better handling
fixtures_training <- fixtures_training %>% 
  mutate(across(any_of(post_game_varnames), as.numeric))

# FEATURE ENGINEERING
#Functions

fixtures_training <- fixtures_training %>% arrange(desc(fixture.date))

avg_df <- calculate_averages(fixtures_training, post_game_varnames, 
                             fixture_lookback = 5,
                             weighting = "normal")

fixtures_training <- calculate_time_diff(fixtures_training)
# games_played_recent <- 
namefile <- paste0(Sys.Date(), "__", "avg_df")
save(avg_df, file = namefile)
# setting the output variable
target_variable <- ifelse(is.na(fixtures_training$teams.home.winner), "draw", 
                          ifelse(fixtures_training$teams.home.winner == 1, "home",
                                 ifelse(is.na(fixtures_training$teams.home.winner), "draw", 
                                        ifelse(fixtures_training$teams.home.winner == 0, "away", NA))))
fixtures_training$outcome <- target_variable
fixtures_training$outcome <- as.factor(target_variable)
# de-select forward-looking bias variables, and add aggregated post_games
full_set <- left_join(fixtures_training[, !names(fixtures_training) %in% post_game_varnames], avg_df,
                      by = "fixture.id") %>% 
# full_set <- bind_cols(fixtures_training[, !names(fixtures_training) %in% post_game_varnames],avg_df) %>% 
  select(-any_of(redundant_vars)) %>% 
  suppressMessages()



# CLEANING
# Filter for missing values and correct match status
# remove full NaN column
clean_set <- full_set[, colSums(is.na(full_set)) < nrow(full_set)]
# remove dirty rows with very little data
clean_set <- clean_set[rowSums(is.na(clean_set)) <= 105, ]
# clean_set %>%
#   summarise(across(everything(), ~ sum(is.na(.))))
# impute the missing averages with a median
clean_set <- clean_set %>% 
  mutate(across(where(is.numeric), ~replace_na(., mean(., na.rm=TRUE))))

character_cols <- sapply(clean_set, is.character)
clean_set[, character_cols][is.na(clean_set[, character_cols])] <- "no-value"
clean_set[is.na(clean_set)] <- -1
# only want finished games included - forward-looking variable
clean_set <- clean_set[clean_set$fixture.status.long == "Match Finished",]
clean_set <- clean_set %>% select(-c(fixture.status.long, fixture.status.elapsed))


# REGRESSION TREE
save.image(file = paste0(Sys.Date(),"ready_for_execution_4thparty.RData"))
# load("2024-06-30ready_for_execution.RData")
forest_spec <- rand_forest(trees = 10000, 
                           min_n = 1, 
                           mtry = round(sqrt(ncol(clean_set))), 
                           mode = "classification") %>% 
  set_engine("ranger", importance = "impurity", max.depth = 100)

split_set <- initial_split(clean_set, prop = 0.75, strata =outcome)
train_set <- training(split_set)
test_set <- testing(split_set)

x_vars <- train_set %>% 
  select(-outcome)
y_var <- train_set %>% 
  select(outcome)

model <- forest_spec %>% 
  fit_xy(x = x_vars , y = y_var, data = train_set)

predictions <- predict(model, new_data = test_set)

# Create confusion matrix
confusionMatrix(predictions$.pred_class, test_set$outcome)

# INTERPRET
tree_model <- extract_fit_engine(model)
vip(tree_model, num_features = ncol(clean_set)-1)

