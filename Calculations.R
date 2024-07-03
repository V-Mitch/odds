# Custom Functions
library(stringr)

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


calculate_averages <- function(fixture_dataframe, 
                               post_game_varnames, 
                               fixture_lookback = 5,
                               weighting = "normal"){
  post_game_varnames <- post_game_varnames[post_game_varnames %in% colnames(fixture_dataframe)]
  
  # arrange by date chronologically youngest to oldest game
  fixture_dataframe <- fixture_dataframe %>%
    arrange(desc(fixture.date))
  df_averages <- data.frame(matrix(NA, ncol = length(post_game_varnames)*3-2, nrow = nrow(fixture_dataframe)))
  
  weights <- switch(weighting,
                    "normal" = rep(1, length.out = fixture_lookback),
                    "linear" = seq(1, 0, length.out = fixture_lookback),
                    "exponential" = exp(seq(-1, -fixture_lookback, 
                                            length.out = fixture_lookback))
  )
  # Which variables should have the difference taken?
  
  comparison_vars_t1 <- post_game_varnames[grepl("team\\.1\\.|team\\.2\\.", post_game_varnames)]
  comparison_vars_t3 <- post_game_varnames[grepl("teams", post_game_varnames)]
  comparison_vars_t2 <- post_game_varnames[!post_game_varnames %in% 
                                             c(comparison_vars_t1, comparison_vars_t3)]
  unique_vars_t1 <- unique(sub("team\\.[12]\\.", "", comparison_vars_t1))
  unique_vars_t2 <- gsub("\\.home|\\.away", "", comparison_vars_t2)
  # unique_vars_t3 <- gsub("\\.home|\\.away", "", comparison_vars_t3)
  if (length(unique_vars_t1) > 0){
    diff_vars_t1 <- paste0("diff.", unique_vars_t1)
  } else {
    diff_vars_t1 <- c()
  }
  diff_vars_t2 <- paste0("diff.", unique_vars_t2)
  
  swap_column_names <- function(df) {
    names(df) <- gsub("1", "temp", names(df))
    names(df) <- gsub("2", "1", names(df))
    names(df) <- gsub("temp", "2", names(df))
    names(df) <- gsub("home", "temp", names(df))
    names(df) <- gsub("away", "home", names(df))
    names(df) <- gsub("temp", "away", names(df))
    return(df)
  }
  
  for (i in 1:nrow(fixture_dataframe)){
    remaining_fixtures <- fixture_dataframe[i:nrow(fixture_dataframe),]
    current_fixture <- remaining_fixtures %>%
      slice(1)
    home_team <- current_fixture["teams.home.name"]
    away_team <- current_fixture["teams.away.name"]
    home_prior_5 <- remaining_fixtures %>%
      filter(teams.home.name == as.character(home_team) | 
               teams.away.name == as.character(home_team)) %>%
      slice(-1) %>% 
      slice_head(n = fixture_lookback)
    
    away_prior_5 <- remaining_fixtures %>%
      filter(teams.home.name == as.character(away_team) | 
               teams.away.name == as.character(away_team)) %>%
      slice(-1) %>% 
      slice_head(n = fixture_lookback)

    home_prior_as_away  <- home_prior_5 %>%
      filter(teams.away.name == as.character(home_team)) %>% 
      swap_column_names()
    home_prior_as_home <- home_prior_5 %>%
      filter(teams.home.name == as.character(home_team))
    home_prior_5 <- bind_rows(home_prior_as_home, home_prior_as_away) %>% 
      arrange(desc(fixture.date))
    
    away_prior_as_away  <- away_prior_5 %>%
      filter(teams.away.name == as.character(away_team)) %>% 
      swap_column_names()
    away_prior_as_home <- away_prior_5 %>%
      filter(teams.home.name == as.character(away_team))
    away_prior_5 <- bind_rows(away_prior_as_home, away_prior_as_away) %>% 
      arrange(desc(fixture.date))
    # Calculate the average of yellow cards for home home when they are the home team
    # Use a loop to mutate the data frame, adding new columns for each difference
    for (j in seq_along(unique_vars_t1)) {
      var_temp <- unique_vars_t1[j]
      home_prior_5 <- home_prior_5 %>%
        mutate(!!diff_vars_t1[j] := .data[[paste0("team.1.", var_temp)]] - .data[[paste0("team.2.", var_temp)]])
      away_prior_5 <- away_prior_5 %>%
        mutate(!!diff_vars_t1[j] := .data[[paste0("team.1.", var_temp)]] - .data[[paste0("team.2.", var_temp)]])
    }
    for (j in seq_along(unique_vars_t2)) {
      var_temp <- unique_vars_t2[j]
      var_temp <- gsub("\\.(home|away)", "", var_temp)
      home_prior_5 <- home_prior_5 %>%
        mutate(!!diff_vars_t2[j] := .data[[paste0(var_temp,".home")]] - 
                 .data[[paste0(var_temp, ".away")]])
      away_prior_5 <- away_prior_5 %>%
        mutate(!!diff_vars_t2[j] := .data[[paste0(var_temp,".home")]] - 
                 .data[[paste0(var_temp, ".away")]])
    }

    if(nrow(home_prior_5)>0){
    weights_temp <- weights[1:nrow(home_prior_5)]
    avgs_for_home_team <- home_prior_5 %>% 
      summarise(across(all_of(c(post_game_varnames,
                                diff_vars_t1, diff_vars_t2)), 
                       ~ weighted.mean(., w = weights_temp, na.rm = TRUE)))
    }else{
      column_names <- unique(c(post_game_varnames,
                               diff_vars_t1, diff_vars_t2))
      df <- data.frame(matrix(ncol = length(column_names), nrow = 0))
      colnames(df) <- column_names
      avgs_for_home_team <- df %>% add_row()
    }
    if(nrow(away_prior_5)>0){
    weights_temp <- weights[1:nrow(away_prior_5)]
    avgs_for_away_team <- away_prior_5 %>% 
      summarise(across(all_of(c(post_game_varnames,
                                diff_vars_t1, diff_vars_t2)), 
                       ~ weighted.mean(., w = weights_temp, na.rm = TRUE)))
    }else{
      column_names <- unique(c(post_game_varnames,
                               diff_vars_t1, diff_vars_t2))
      df <- data.frame(matrix(ncol = length(column_names), nrow = 0))
      colnames(df) <- column_names
      avgs_for_away_team <- df %>% add_row()
    }
    colnames(avgs_for_home_team) <- paste0("crthome.", colnames(avgs_for_home_team))
    colnames(avgs_for_away_team) <- paste0("crtaway.", colnames(avgs_for_away_team))
    colnames(df_averages) <- c(paste0(colnames(avgs_for_home_team)),
                               paste0(colnames(avgs_for_away_team)))

    df_averages <- bind_rows(df_averages,
                             bind_cols(avgs_for_home_team, avgs_for_away_team)) %>% 
      suppressMessages() 
    
    print(paste0("Calculated averages for fixture ",i,"/",nrow(fixture_dataframe)))
    
  }
  colnames(df_averages) <- c(paste0(colnames(avgs_for_home_team)),
                          paste0(colnames(avgs_for_away_team)))
  return(df_averages)
}

calculate_time_diff <- function(fixture_dataframe){
  fixture_dataframe <- fixture_dataframe %>%
    arrange(fixture.timestamp) %>%  # Make sure data is sorted by timestamp
    group_by(teams.home.name) %>%  # Group by home team name
    mutate(
      teams.home.since.last = fixture.timestamp - lag(fixture.timestamp, default = first(fixture.timestamp)),
      teams.home.since.last = ifelse(is.na(teams.home.since.last), NaN, teams.home.since.last)
    ) %>%
    ungroup() %>%
    arrange(fixture.timestamp) %>%  # Re-arrange by timestamp again
    group_by(teams.away.name) %>%  # Group by away team name
    mutate(
      teams.away.since.last = fixture.timestamp - lag(fixture.timestamp, default = first(fixture.timestamp)),
      teams.away.since.last = ifelse(is.na(teams.away.since.last), NaN, teams.away.since.last)
    ) %>%
    ungroup()
  return(fixture_dataframe)
}

# home_away_flipper


# calculate_recent_games <- function(fixture_dataframe, 
#                                    days_lookback = 365,
#                                    weighting = "normal"){
#   weights <- switch(weighting,
#                     "normal" = seq(1, length.out = fixture_lookback),
#                     "linear" = seq(1, 0, length.out = fixture_lookback),
#                     "exponential" = exp(seq(-1, -fixture_lookback, 
#                                             length.out = fixture_lookback))
#   )
#   for (i in 1:nrow(fixture_dataframe)){
#     remaining_fixtures <- fixture_dataframe[i:nrow(fixtures_training),]
#     current_fixture_timestamp <- fixture_dataframe[i, fixture.timestamp]
#     
#     recent_fixtures <- remaining_fixtures %>%
#       slice(-1) %>% 
#       filter(fixture.timestamp >= current_fixture_timestamp - 
#                ((24 * 60 * 60) * days_lookback))
#              current_fixture <- remaining_fixtures %>%
#                slice(1)
#              
#              home_team <- current_fixture["teams.home.name"]
#              away_team <- current_fixture["teams.away.name"]
#              
#              # Calculate the average of yellow cards for home home when they are the home team
#              home_avg_games <- recent_fixtures %>%
#                filter(teams.home.name == home_team) %>%
#                slice(-1) %>% 
#                
#              
#              # Calculate the average of yellow cards for away team when they are the away team
#              away_avg_games <- remaining_fixtures %>%
#                filter(teams.away.name == away_team) %>%
#                slice(-1) %>% 
#                slice_head(n = fixture_lookback) %>% 
#                summarise(across(all_of(post_game_varnames), ~ weighted.mean(., w = weights, na.rm = TRUE)))
#              
#              df_averages[i,] <- bind_cols(home_avg_prior_5, away_avg_prior_5) %>% 
#                suppressMessages()
#              
#              print(paste0("Calculated averages for fixture ",i,"/",nrow(fixture_dataframe)))
#   }
#   colnames(df_averages) <- c(paste0("current.team.1.avg.prev.",fixture_lookback,".", post_game_varnames),
#                              paste0("current.team.2.avg.prev.",fixture_lookback,".", post_game_varnames))
#   return(df_averages)
# }


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
                    "teams.home.logo",
                    "teams.away.logo")
