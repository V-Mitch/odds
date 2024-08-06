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


get_past_matches <- function(leagues, seasons, api_key, api_host, teams = c(), short_limit = 300){
  
  url <- paste(infinitif,"/fixtures/?league=",leagues,"&season=",seasons,"", sep = "")
  res <- GET(url,  add_headers('x-rapidapi-key' = api_key,
                               'x-rapidapi-host' = api_host))
  str_res_json <- rawToChar(res$content)
  parsed_fixtures <- fromJSON(str_res_json)
  tib_df <- flatten_to_df(parsed_fixtures$response)
  
  if (!is.null(teams)){
    tib_df <- tib_df %>% filter(teams.away.name %in% teams | teams.home.name %in% teams)
  }
  
  fixture_df <- data.frame(matrix(ncol = 0, nrow = 1))
  
  time_loop_start <- Sys.time()
  
  for (i in 1:dim(tib_df)[1]){
    
    url <- paste(infinitif,"/fixtures/statistics/?fixture=",tib_df[i,"fixture.id"], sep = "")
    res <- GET(url,  add_headers('x-rapidapi-key' = api_key,
                                 'x-rapidapi-host' = api_host))
    str_res_json <- rawToChar(res$content)
    parsed_stats <- fromJSON(str_res_json)
    if (length(parsed_stats$response) !=0){
      stats_df <- flatten_fixture_stats(parsed_stats$response) %>% select(-logo, -id) %>% t() 
      colnames(stats_df) <- paste(stats_df['name',],stats_df['type',], sep = ".")
      stats_df <- t(stats_df[!rownames(stats_df) %in% c("name", "type"),])
    } else {
      stats_df <- data.frame(matrix(ncol = 0, nrow = 1))
    }
    
    # final_fixture_row <- cbind(tib_df[i,], lineup_df, stats_df)
    final_fixture_row <- cbind(tib_df[i,], stats_df)
    fixture_df <- bind_rows(final_fixture_row, fixture_df) %>% 
      suppressMessages()
    print(paste0("Fixture ", i ," added to the fixture dataframe. ",res$headers$`x-ratelimit-requests-remaining` ,
                 "/", res$headers$`x-ratelimit-requests-limit`,
                 " tokens remaining from API-Odds. Dimensions of current dataframe: ", 
                 paste0(dim(fixture_df)[1], " rows, ",dim(fixture_df)[2])," columns."))
    if (i %% short_limit/2 == 0 & Sys.time() - time_loop_start < 60){
      time_loop_start <- Sys.time()
      print(paste0("pausing for ", 60 - (Sys.time() - time_loop_start) + 1, " seconds"))
      Sys.sleep(60 - (Sys.time() - time_loop_start) + 1)
    }
  }
  return(fixture_df)
}


get_past_players <- function(leagues, seasons, api_key, api_host, teams = c(), short_limit = 300){
  
  url <- paste(infinitif,"/fixtures/?league=",leagues,"&season=",seasons,"", sep = "")
  res <- GET(url,  add_headers('x-rapidapi-key' = api_key,
                               'x-rapidapi-host' = api_host))
  str_res_json <- rawToChar(res$content)
  parsed_fixtures <- fromJSON(str_res_json)
  tib_df <- flatten_to_df(parsed_fixtures$response)
  
  if (!is.null(teams)){
    tib_df <- tib_df %>% filter(teams.away.name %in% teams | teams.home.name %in% teams)
  }
  
  fixture_df <- data.frame(matrix(ncol = 0, nrow = 1))
  
  time_loop_start <- Sys.time()
  
  for (i in 1:nrow(tib_df)){
    # for (i in 1:10){
    
    # browser()
    
    url <- paste(infinitif,"/fixtures/lineups/?fixture=",tib_df[i,"fixture.id"], sep = "")
    res <- GET(url,  add_headers('x-rapidapi-key' = api_key,
                                 'x-rapidapi-host' = api_host))
    # obtain the lineup of the players from each fixture 
    str_res_json <- rawToChar(res$content)
    parsed_lineups <- fromJSON(str_res_json)
    if (length(parsed_lineups$response) !=0){
      lineup_df <- flatten_to_df_lineup(parsed_lineups$response)
    } else{
      lineup_df <- data.frame(matrix(ncol = 0, nrow = 1))
    }
    # obtain the rating of the players during each fixture 
    url <- paste(infinitif,"/fixtures/players/?fixture=",tib_df[i,"fixture.id"], sep = "")
    res <- GET(url,  add_headers('x-rapidapi-key' = api_key,
                                 'x-rapidapi-host' = api_host))
    str_res_json <- rawToChar(res$content)
    parsed_lineups <- fromJSON(str_res_json)
    if (length(parsed_lineups$response) !=0){
      player_stats_df <- flatten_to_df_rating(parsed_lineups$response)
    } else{
      player_stats_df <- data.frame(matrix(ncol = 0, nrow = 1))
    }
    
    # add the statistics of each player to the player lineups
    omit_names <- c("team.id", "team.name", "team.logo", "update", "id", "name", "photo")
    player_stats_names <- names(player_stats_df)[!names(player_stats_df) %in% omit_names]
    player_id_cols <- grep("player.id", names(lineup_df), value = TRUE)
    for (j in 1:length(player_stats_names)){
      name_of_metric <- player_stats_names[j]
      for (k in 1:length(player_id_cols)){
        name_of_column <- gsub("id", name_of_metric, player_id_cols[k])
        temp_data <- player_stats_df[player_stats_df[,"id"] == as.numeric(lineup_df[,player_id_cols][k]) , ][name_of_metric]
        lineup_df[name_of_column] <- temp_data
      }
    }
    
    final_fixture_row <- cbind(tib_df[i,], lineup_df)
    fixture_df <- bind_rows(final_fixture_row, fixture_df) %>% 
      suppressMessages()
    print(paste0("Fixture ", i ," added to the fixture dataframe. ",res$headers$`x-ratelimit-requests-remaining` ,
                 "/", res$headers$`x-ratelimit-requests-limit`,
                 " tokens remaining from API-Odds. Dimensions of current dataframe: ", 
                 paste0(dim(fixture_df)[1], " rows, ",dim(fixture_df)[2])," columns."))
    if (i %% short_limit/2 == 0 & Sys.time() - time_loop_start < 60){
      time_loop_start <- Sys.time()
      print(paste0("pausing for ", 60 - (Sys.time() - time_loop_start) + 1, " seconds"))
      Sys.sleep(60 - (Sys.time() - time_loop_start) + 1)
    }
  }
  return(fixture_df)
}