# master script for project execution (temporary)

source("repo_class.R")
source("DataHandlers.R")
source("Calculations.R")

sports_repo = new("Sports_Repository")
sports_repo = updateSportsFromAPI(sports_repo, all = "false")
sports_repo@data$title

events_repo = new("Events_Repository")
events_repo = updateEventsFromAPI(events_repo, sport = "soccer_uefa_european_championship")


odds_repo = new("Odds_Repository")
odds_repo = updateOddsFromAPI(odds_repo, sport = "soccer_uefa_european_championship", market = "h2h", regions = "uk,eu")

odds_table = extract_over_row(odds_repo@data)

teams_repo = new("Teams_Repository")
teams_repo = updateTeamsFromAPI(teams_repo, competition = "EC")



library(data.table)


# Sample data, assuming 'data' is your input dataframe
# data <- fread("your_file.csv")  # Load your data appropriately


