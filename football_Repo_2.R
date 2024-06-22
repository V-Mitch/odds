library(jsonlite)
library(httr)
library(rjson)
library(data.table)
library(purrr)
library(dplyr)
library(tidyr)
library(caret)
source("stack_player_data.R")

# queryString <- list(
#   league = "39",
#   season = "2020",
#   team = "33"
# )
# 
# url <- "https://api-football-v1.p.rapidapi.com/v3/teams/statistics"
# res <- GET(url,  add_headers('x-rapidapi-key' = "9ae62b4f01msh4629643f4c38fc4p120e63jsn812161bafbe6",
#                              'x-rapidapi-host' = 'api-football-v1.p.rapidapi.com'),
#            query = queryString)
# str_res_json <- rawToChar(res$content)
# parsed_data <- fromJSON(str_res_json)
# parsed_data
# 
# 
# # rbindlist(lapply(parsed_data$response, as.data.table))
# # MISCELLANEOUS
# url <- "https://api-football-v1.p.rapidapi.com/v3/teams/?league=4&season=2024"
# res <- GET(url,  add_headers('x-rapidapi-key' = "9ae62b4f01msh4629643f4c38fc4p120e63jsn812161bafbe6",
#                              'x-rapidapi-host' = 'api-football-v1.p.rapidapi.com'))
# str_res_json <- rawToChar(res$content)
# euro_cup_teams <- fromJSON(str_res_json)
# euro_cup_teams
# 
# 
# 
# url <- "https://api-football-v1.p.rapidapi.com/v3/leagues"
# res <- GET(url,  add_headers('x-rapidapi-key' = "9ae62b4f01msh4629643f4c38fc4p120e63jsn812161bafbe6",
#                              'x-rapidapi-host' = 'api-football-v1.p.rapidapi.com'))
# str_res_json <- rawToChar(res$content)
# leagues_data <- fromJSON(str_res_json)
# leagues_data
# 
# url <- paste("https://api-football-v1.p.rapidapi.com/v3/players/?league=10&season=2024&page=",1,sep = "")
# res <- GET(url,  add_headers('x-rapidapi-key' = "9ae62b4f01msh4629643f4c38fc4p120e63jsn812161bafbe6",
#                              'x-rapidapi-host' = 'api-football-v1.p.rapidapi.com'))
# str_res_json <- rawToChar(res$content)
# parsed_friendlies <- fromJSON(str_res_json)


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


get_past_matches <- function(leagues, seasons, teams, api_key, api_host){
  
  url <- paste(infinitif,"/fixtures/?league=",leagues,"&season=",seasons,"", sep = "")
  res <- GET(url,  add_headers('x-rapidapi-key' = api_key,
                               'x-rapidapi-host' = api_host))
  str_res_json <- rawToChar(res$content)
  parsed_fixtures <- fromJSON(str_res_json)
  tib_df <- flatten_to_df(parsed_fixtures$response)
  tib_df <- tib_df %>% filter(teams.away.name %in% teams | teams.home.name %in% teams)
  for (i in 1:dim(tib_df)[1]){
    
    url <- paste(infinitif,"/fixtures/lineups/?fixture=",tib_df[i,"fixture.id"], sep = "")
    res <- GET(url,  add_headers('x-rapidapi-key' = api_key,
                                 'x-rapidapi-host' = api_host))
    str_res_json <- rawToChar(res$content)
    parsed_lineups <- fromJSON(str_res_json)
    lineup_df <- flatten_to_df_lineup(parsed_lineups$response)
    
    url <- paste(infinitif,"/fixtures/statistics/?fixture=",tib_df[i,"fixture.id"], sep = "")
    res <- GET(url,  add_headers('x-rapidapi-key' = api_key,
                                 'x-rapidapi-host' = api_host))
    str_res_json <- rawToChar(res$content)
    parsed_stats <- fromJSON(str_res_json)
    stats_df <- flatten_fixture_stats(parsed_stats$response) %>% select(-logo, -id) %>% t() 
    colnames(stats_df) <- paste(stats_df['name',],stats_df['type',], sep = ".")
    stats_df <- t(stats_df[!rownames(stats_df) %in% c("name", "type"),])
    
    url <- paste(infinitif,"/odds/?fixture=",tib_df[i,"fixture.id"], sep = "")
    res <- GET(url,  add_headers('x-rapidapi-key' = api_key,
                                 'x-rapidapi-host' = api_host))
    str_res_json <- rawToChar(res$content)
    parsed_odds <- fromJSON(str_res_json)
    
    final_fixture_row <- cbind(tib_df[i,], lineup_df, stats_df)
    
  }
}


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


get_past_matches <- function(leagues, seasons, teams, api_key, api_host){
  
  url <- paste(infinitif,"/fixtures/?league=",leagues,"&season=",seasons,"", sep = "")
  res <- GET(url,  add_headers('x-rapidapi-key' = api_key,
                               'x-rapidapi-host' = api_host))
  str_res_json <- rawToChar(res$content)
  parsed_fixtures <- fromJSON(str_res_json)
  tib_df <- flatten_to_df(parsed_fixtures$response)
  tib_df <- tib_df %>% filter(teams.away.name %in% teams | teams.home.name %in% teams)
  for (i in 1:dim(tib_df)[1]){
    
    url <- paste(infinitif,"/fixtures/lineups/?fixture=",tib_df[i,"fixture.id"], sep = "")
    res <- GET(url,  add_headers('x-rapidapi-key' = api_key,
                                 'x-rapidapi-host' = api_host))
    str_res_json <- rawToChar(res$content)
    parsed_lineups <- fromJSON(str_res_json)
    lineup_df <- flatten_to_df_lineup(parsed_lineups$response)
    
    url <- paste(infinitif,"/fixtures/statistics/?fixture=",tib_df[i,"fixture.id"], sep = "")
    res <- GET(url,  add_headers('x-rapidapi-key' = api_key,
                                 'x-rapidapi-host' = api_host))
    str_res_json <- rawToChar(res$content)
    parsed_stats <- fromJSON(str_res_json)
    stats_df <- flatten_fixture_stats(parsed_stats$response) %>% select(-logo, -id) %>% t() 
    colnames(stats_df) <- paste(stats_df['name',],stats_df['type',], sep = ".")
    stats_df <- stats_df[!rownames(stats_df) %in% c("name", "type"),]
    
    
  }
}

#fixtures plain
url <- paste("https://api-football-v1.p.rapidapi.com/v3/fixtures/?league=10&season=2024")
res <- GET(url,  add_headers('x-rapidapi-key' = "9ae62b4f01msh4629643f4c38fc4p120e63jsn812161bafbe6",
                             'x-rapidapi-host' = 'api-football-v1.p.rapidapi.com'))
str_res_json <- rawToChar(res$content)
parsed_fixtures <- fromJSON(str_res_json)
parsed_fixtures$response[[1]]
parsed_fixtures$response[[1]]$fixture$id
# fixtures lineup
url <- paste("https://api-football-v1.p.rapidapi.com/v3/fixtures/lineups/?fixture=1155234")
res <- GET(url,  add_headers('x-rapidapi-key' = "9ae62b4f01msh4629643f4c38fc4p120e63jsn812161bafbe6",
                             'x-rapidapi-host' = 'api-football-v1.p.rapidapi.com'))
str_res_json <- rawToChar(res$content)
parsed_lineups <- fromJSON(str_res_json)
parsed_lineups[[1]]
# fixtures stats
url <- paste("https://api-football-v1.p.rapidapi.com/v3/fixtures/statistics/?fixture=1155262")
res <- GET(url,  add_headers('x-rapidapi-key' = "9ae62b4f01msh4629643f4c38fc4p120e63jsn812161bafbe6",
                             'x-rapidapi-host' = 'api-football-v1.p.rapidapi.com'))
str_res_json <- rawToChar(res$content)
parsed_stats <- fromJSON(str_res_json)
parsed_stats$response[[1]]
# fixtures h2h (need 2 teams)
url <- paste("https://api-football-v1.p.rapidapi.com/v3/fixtures/headtohead/?fixture=1155234")
res <- GET(url,  add_headers('x-rapidapi-key' = "9ae62b4f01msh4629643f4c38fc4p120e63jsn812161bafbe6",
                             'x-rapidapi-host' = 'api-football-v1.p.rapidapi.com'))
str_res_json <- rawToChar(res$content)
parsed_h2h <- fromJSON(str_res_json)
parsed_h2h$response[[1]]
# fixtures players
url <- paste("https://api-football-v1.p.rapidapi.com/v3/fixtures/players/?fixture=1155262")
res <- GET(url,  add_headers('x-rapidapi-key' = "9ae62b4f01msh4629643f4c38fc4p120e63jsn812161bafbe6",
                             'x-rapidapi-host' = 'api-football-v1.p.rapidapi.com'))
str_res_json <- rawToChar(res$content)
parsed_players <- fromJSON(str_res_json)
parsed_players$response[[1]]
# fixtures prediction
url <- paste("https://api-football-v1.p.rapidapi.com/v3/predictions/?fixture=1155262")
res <- GET(url,  add_headers('x-rapidapi-key' = "9ae62b4f01msh4629643f4c38fc4p120e63jsn812161bafbe6",
                             'x-rapidapi-host' = 'api-football-v1.p.rapidapi.com'))
str_res_json <- rawToChar(res$content)
parsed_prediction <- fromJSON(str_res_json)
parsed_prediction$response[[1]]

# GET ALL THE TEAMS FROM EURO CHAMPIONSHIP (LEAGUE = 4) AND SEASON 2024
# FRIENDLIES (LEAGUE = 10)

i = 1
url <- paste("https://api-football-v1.p.rapidapi.com/v3/players/?league=10&season=2023&page=",i,sep = "")
res <- GET(url,  add_headers('x-rapidapi-key' = "9ae62b4f01msh4629643f4c38fc4p120e63jsn812161bafbe6",
                             'x-rapidapi-host' = 'api-football-v1.p.rapidapi.com'))
str_res_json <- rawToChar(res$content)
parsed_data <- fromJSON(str_res_json)
if(parsed_data$paging$total > 1){
  full_book <- stack_player_data(parsed_data)
  for (i in 2:parsed_data$paging$total){
    # Call API for players
    url <- paste("https://api-football-v1.p.rapidapi.com/v3/players/?league=4&season=2024&page=",i,sep = "")
    res <- GET(url,  add_headers('x-rapidapi-key' = "9ae62b4f01msh4629643f4c38fc4p120e63jsn812161bafbe6",
                                 'x-rapidapi-host' = 'api-football-v1.p.rapidapi.com'))
    str_res_json <- rawToChar(res$content)
    parsed_data <- fromJSON(str_res_json)
    data_current_page <- stack_player_data(parsed_data)
    full_book <- rbind(full_book, data_current_page)
  }
} else {
  full_book <- stack_player_data(parsed_data)
}