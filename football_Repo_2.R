library(jsonlite)
library(httr)
library(rjson)
library(data.table)
library(purrr)
library(dplyr)
library(tidyr)
source("stack_player_data.R")

queryString <- list(
  league = "39",
  season = "2020",
  team = "33"
)

url <- "https://api-football-v1.p.rapidapi.com/v3/teams/statistics"
res <- GET(url,  add_headers('x-rapidapi-key' = "9ae62b4f01msh4629643f4c38fc4p120e63jsn812161bafbe6",
                             'x-rapidapi-host' = 'api-football-v1.p.rapidapi.com'),
           query = queryString)
str_res_json <- rawToChar(res$content)
parsed_data <- fromJSON(str_res_json)
parsed_data


# rbindlist(lapply(parsed_data$response, as.data.table))


url <- "https://api-football-v1.p.rapidapi.com/v3/teams/?league=4&season=2024"
res <- GET(url,  add_headers('x-rapidapi-key' = "9ae62b4f01msh4629643f4c38fc4p120e63jsn812161bafbe6",
                             'x-rapidapi-host' = 'api-football-v1.p.rapidapi.com'))
str_res_json <- rawToChar(res$content)
euro_cup_teams <- fromJSON(str_res_json)
euro_cup_teams



url <- "https://api-football-v1.p.rapidapi.com/v3/leagues"
res <- GET(url,  add_headers('x-rapidapi-key' = "9ae62b4f01msh4629643f4c38fc4p120e63jsn812161bafbe6",
                             'x-rapidapi-host' = 'api-football-v1.p.rapidapi.com'))
str_res_json <- rawToChar(res$content)
leagues_data <- fromJSON(str_res_json)
leagues_data

url <- paste("https://api-football-v1.p.rapidapi.com/v3/players/?league=10&season=2024&page=",1,sep = "")
res <- GET(url,  add_headers('x-rapidapi-key' = "9ae62b4f01msh4629643f4c38fc4p120e63jsn812161bafbe6",
                             'x-rapidapi-host' = 'api-football-v1.p.rapidapi.com'))
str_res_json <- rawToChar(res$content)
parsed_friendlies <- fromJSON(str_res_json)


# GET ALL THE TEAMS FROM EURO CHAMPIONSHIP (LEAGUE = 4) AND SEASON 2024
# FRIENDLIES (LEAGUE = 10)

i = 1
url <- paste("https://api-football-v1.p.rapidapi.com/v3/players/?league=4&season=2024&page=",i,sep = "")
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