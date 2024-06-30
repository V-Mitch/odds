# Custom Functions

fairing_odds_ <- function(odds1, odds2, odds3){
  markup <- sum(1/odds1, 1/odds2, 1/odds3) - 1
  fair1 <- odds1*(1+markup)
  fair2 <- odds2*(1+markup)
  fair3 <- odds3*(1+markup)
  return(list(fair1,fair2,fair3, markup))
}

fairing_odds <- function(...){
  args <- as.numeric(list(...))
  total_sum <- 0
  for (arg in args){
      total_sum <- (total_sum + 1/arg)
    }
    markup <- total_sum - 1
    fair_list <- c(rep(NaN,length(args)))
  for (i in 1:length(fair_list)){
    fair_list[i] <- (args[[i]]*(1+markup))
  }
    res = as.data.frame(
      matrix(
        c(
          args, markup,
          fair_list, 0,
          1/args, 1-sum(1/args),
          1/fair_list, 0
          ),
        nrow = 4, 
        byrow = TRUE
      ) 
    )
    rownames(res) <- c("Original", "M_Fair", "Original_Prob", "M_Fair_Prob")
    colnames(res) <- c(paste("price_", 1:(ncol(res)-1), sep = ""), "markup")
    return(res)
}
  
probablize <- function(oddsvector){
  pct1 <- 1/oddsvector[1]
  pct2 <- 1/oddsvector[2]
  pct3 <- 1/oddsvector[3]
  return(list(pct1,pct2,pct3))
}

calc_row <- function(data){
  
  res <- fairing_odds(data)  

  
}


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
      slice(-1) %>% 
      slice_head(n = fixture_lookback) %>% 
      summarise(across(all_of(post_game_varnames), ~ mean(., na.rm = TRUE)))
    # colnames(home_avg_prior_5) <- paste0("avg.prev.5.","home", 
    #                                      colnames(home_avg_prior_5))
    # Calculate the average of yellow cards for away team when they are the away team
    away_avg_prior_5 <- remaining_fixtures %>%
      filter(teams.away.name == away_team) %>%
      slice(-1) %>% 
      slice_head(n = fixture_lookback) %>% 
      summarise(across(all_of(post_game_varnames), ~ mean(., na.rm = TRUE)))
    # colnames(away_avg_prior_5) <- paste0("avg.prev.5.","away", 
    #                                      colnames(away_avg_prior_5))
    
    df_averages[i,] <- bind_cols(home_avg_prior_5, away_avg_prior_5) %>% 
      suppressMessages()

    print(paste0("Calculated averages for fixture ",i,"/",nrow(fixture_dataframe)))
  }
  colnames(df_averages) <- c(paste0("current.team.1.avg.prev.",fixture_lookback,".", post_game_varnames),
                             paste0("current.team.2.avg.prev.",fixture_lookback,".", post_game_varnames))
  return(df_averages)
}

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
                        "team.2.goals_prevented"    ,
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

redundant_vars <- c("fixture.timezone", 
                        "fixture.venue.id",
                        "fixture.status.short",
                        "league.id",
                        "league.logo",
                        "league.flag",
                        "teams.home.id",
                        "teams.away.id",
                        "teams.home.logo")