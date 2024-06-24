# master script for project execution (temporary)

source("repo_class.R")
source("DataHandlers.R")
source("Calculations.R")
source("football_Repo_2.R")

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

# Get list of games in the past
api_key <- "9ae62b4f01msh4629643f4c38fc4p120e63jsn812161bafbe6"
api_host <- "api-football-v1.p.rapidapi.com"
infinitif <- "https://api-football-v1.p.rapidapi.com/v3"
seasons <- 2024
leagues <- 10
teams <- teams_euro2024

fixtures_training <- get_past_matches(leagues, seasons, teams, api_key, api_host, short_limit = 300)




library(data.table)


# Sample data, assuming 'data' is your input dataframe
# data <- fread("your_file.csv")  # Load your data appropriately


