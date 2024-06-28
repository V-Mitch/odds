# master script for project execution (temporary)

source("repo_class.R")
source("DataHandlers.R")
source("Calculations.R")
source("football_Repo_2.R")
library(tidymodels)
library(rpart.plot)
library(caret)

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


# Get list of games in the past 2024
api_key <- "9ae62b4f01msh4629643f4c38fc4p120e63jsn812161bafbe6"
api_host <- "api-football-v1.p.rapidapi.com"
infinitif <- "https://api-football-v1.p.rapidapi.com/v3"
seasons <- 2024
leagues <- 10
fixtures_training_2024 <- get_past_matches(leagues, seasons, api_key, api_host, short_limit = 300)
namefile <- paste0(Sys.Date(), "__", seasons,"_", "leagues")
save(fixtures_training_2024, file = namefile)
# load("2024-06-26__2024_leagues.RData")
Sys.sleep(60 + 1)


# Get list of games in the past 2023
api_key <- "9ae62b4f01msh4629643f4c38fc4p120e63jsn812161bafbe6"
api_host <- "api-football-v1.p.rapidapi.com"
infinitif <- "https://api-football-v1.p.rapidapi.com/v3"
seasons <- 2023
leagues <- 10
fixtures_training_2023 <- get_past_matches(leagues, seasons, api_key, api_host, short_limit = 300)
namefile <- paste0(Sys.Date(), "__", seasons,"_", "leagues")
save(fixtures_training_2023, file = namefile)
Sys.sleep(60 + 1)


# Get list of games in the past 2022
api_key <- "9ae62b4f01msh4629643f4c38fc4p120e63jsn812161bafbe6"
api_host <- "api-football-v1.p.rapidapi.com"
infinitif <- "https://api-football-v1.p.rapidapi.com/v3"
seasons <- 2022
leagues <- 10
fixtures_training_2022 <- get_past_matches(leagues, seasons, api_key, api_host, short_limit = 300)
namefile <- paste0(Sys.Date(), "__", seasons,"_", "leagues")
save(fixtures_training_2022, file = namefile)
Sys.sleep(60 + 1)

# Get list of games in the past 2021
api_key <- "9ae62b4f01msh4629643f4c38fc4p120e63jsn812161bafbe6"
api_host <- "api-football-v1.p.rapidapi.com"
infinitif <- "https://api-football-v1.p.rapidapi.com/v3"
seasons <- 2021
leagues <- 10
fixtures_training_2021 <- get_past_matches(leagues, seasons, api_key, api_host, short_limit = 300)
namefile <- paste0(Sys.Date(), "__", seasons,"_", "leagues")
save(fixtures_training_2021, file = namefile)
Sys.sleep(60 + 1)

# Get list of games in the past 2020
api_key <- "9ae62b4f01msh4629643f4c38fc4p120e63jsn812161bafbe6"
api_host <- "api-football-v1.p.rapidapi.com"
infinitif <- "https://api-football-v1.p.rapidapi.com/v3"
seasons <- 2020
leagues <- 10
fixtures_training_2020 <- get_past_matches(leagues, seasons, api_key, api_host, short_limit = 300)
namefile <- paste0(Sys.Date(), "__", seasons,"_", "leagues")
save(fixtures_training_2020, file = namefile)
Sys.sleep(60 + 1)

# load("2024-06-26__2024_leagues.RData")
# load("2024-06-26__2024_leagues.RData")
# load("2024-06-26__2024_leagues.RData")
# load("2024-06-26__2024_leagues.RData")
# load("2024-06-26__2024_leagues.RData")


# Combine the different years together
fixtures_training <- bind_rows(fixtures_training_2020, fixtures_training_2021, fixtures_training_2022, 
      fixtures_training_2023, fixtures_training_2024) %>% 
      suppressMessages()


# convert to numeric for better handling
fixtures_training <- fixtures_training %>% 
  mutate(across(all_of(post_game_varnames), as.numeric))

# function-to-be starts here
avg_df <- calculate_averages(fixtures_training, post_game_varnames, 7)
namefile <- paste0(Sys.Date(), "__", "avg_df")
save(avg_df, file = namefile)

# de-select forward-looking bias variables, and add aggregated post_games
full_set <- bind_cols(fixtures_training[, !names(fixtures_training) %in% post_game_varnames],avg_df) %>% 
  suppressMessages()


target_variable <- ifelse(is.na(fixtures_training$teams.home.winner), "draw", 
                          ifelse(fixtures_training$teams.home.winner == 1, "home",
                                 ifelse(is.na(fixtures_training$teams.home.winner), "draw", 
                                        ifelse(fixtures_training$teams.home.winner == 0, "away", NA))))
    
full_set$outcome <- target_variable
full_set$outcome <- as.factor(target_variable)
# not enough levels for certain predictors
# full_set <- full_set %>% select(-fixture.referee)

# REGRESSION TREE
tree_spec <- decision_tree() %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

split_set <- initial_split(full_set, prop = 0.75, strata =outcome)
train_set <- training(split_set)
test_set <- testing(split_set)



model <- tree_spec %>% 
  fit(formula = outcome ~ ., data = train_set)
tree_model <- extract_fit_engine(model)

predict(model, new_data = test_set)
