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
api_key <- ""
api_host <- "api-football-v1.p.rapidapi.com"
infinitif <- "https://api-football-v1.p.rapidapi.com/v3"
seasons <- 2024
leagues <- 10
teams <- teams_euro2024

fixtures_training <- get_past_matches(leagues, seasons, teams, api_key, api_host, short_limit = 300)
namefile <- paste0(Sys.Date(), "__", seasons,"_", "leagues")
save(fixtures_training, file = namefile)

# distinguish the data known post-game
post_game_varnames <- c("team.1.Shots on Goal", 
                        "team.1.Total Shots"        ,                
                        "team.1.Shots insidebox"    ,                  
                        "team.1.Fouls"              ,              
                        "team.1.Offsides"           ,                  
                        "team.1.Yellow Cards"       ,            
                        "team.1.Goalkeeper Saves"   ,              
                        "team.1.Passes accurate"    ,          
                        "team.1.expected_goals"     ,                  
                        "team.2.Shots on Goal"      ,                
                        "team.2.Total Shots"        ,                
                        "team.2.Shots insidebox"    ,                  
                        "team.2.Fouls"              ,              
                        "team.2.Offsides"           ,                  
                        "team.2.Yellow Cards"       ,            
                        "team.2.Goalkeeper Saves"   ,              
                        "team.2.Passes accurate"    ,          
                        "team.2.expected_goals"     ,
                        "team.1.Shots off Goal"     ,             
                        "team.1.Blocked Shots"      ,             
                        "team.1.Shots outsidebox"   ,             
                        "team.1.Corner Kicks"       ,             
                        "team.1.Ball Possession"    ,             
                        "team.1.Red Cards"          ,             
                        "team.1.Total passes"       ,             
                        "team.1.Passes %"           ,             
                        "team.1.goals_prevented"    ,             
                        "team.2.Shots off Goal"     ,             
                        "team.2.Blocked Shots"      ,             
                        "team.2.Shots outsidebox"   ,             
                        "team.2.Corner Kicks"       ,             
                        "team.2.Ball Possession"    ,             
                        "team.2.Red Cards"          ,             
                        "team.2.Total passes"       ,             
                        "team.2.Passes %"           ,
                        "teams.home.winner"         ,
                        "teams.away.winner"         ,
                        "goals.home"                ,                
                        "goals.away"                ,                
                        "score.halftime.home"       ,                
                        "score.halftime.away"       ,                
                        "score.fulltime.home"       ,                
                        "score.fulltime.away"       ,                
                        "score.extratime.home"      ,                
                        "score.extratime.away"      ,                
                        "score.penalty.home"        ,                
                        "score.penalty.away"        )

# convert to numeric for better handling
fixtures_training <- fixtures_training %>% 
  mutate(across(all_of(post_game_varnames), as.numeric))

# function-to-be starts here

calculate_averages <- function(fixture_dataframe, post_game_varnames, fixture_lookback = 5){
  
  
  # arrange by date chronologically youngest to oldest game
  fixture_dataframe <- fixture_dataframe %>%
    arrange(desc(fixture.date))
  df_averages <- data.frame(matrix(NA, ncol = length(post_game_varnames)*2, nrow = nrow(fixture_dataframe)))
  for (i in 1:nrow(fixtures_training)){
    remaining_fixtures <- fixture_dataframe[i:nrow(fixtures_training),]
    
    recent_fixtures <- remaining_fixtures %>%
      slice(-1) %>% 
      slice_head(n = fixture_lookback)
    current_fixture <- remaining_fixtures %>%
      slice(1)
    
    home_team <- current_fixture["teams.home.name"]
    away_team <- current_fixture["teams.away.name"]
    
    
    # Calculate the average of yellow cards for home home when they are the home team
    home_avg_prior_5 <- remaining_fixtures %>%
      filter(teams.home.name == home_team) %>%
      summarise(across(all_of(post_game_varnames), ~ mean(., na.rm = TRUE)))
    # colnames(home_avg_prior_5) <- paste0("avg.prev.5.","home", 
    #                                      colnames(home_avg_prior_5))
    # Calculate the average of yellow cards for away team when they are the away team
    away_avg_prior_5 <- remaining_fixtures %>%
      filter(teams.away.name == away_team) %>%
      summarise(across(all_of(post_game_varnames), ~ mean(., na.rm = TRUE)))
    # colnames(away_avg_prior_5) <- paste0("avg.prev.5.","away", 
    #                                      colnames(away_avg_prior_5))
    
    df_averages[i,] <- bind_cols(home_avg_prior_5, away_avg_prior_5) %>% 
      suppressMessages()
    print(paste0("Calculated averages for fixture ",i,"/",nrow(fixture_training)))
  }
  colnames(df_averages) <- paste0("avg.prev.",fixture_lookback,".", post_game_varnames)
  return(df_averages)
}
avg_df <- calculate_averages(fixtures_training, post_game_varnames, 5)

# remove forward-looking bias variables
fixtures_training[, !names(fixtures_training) %in% post_game_names]
