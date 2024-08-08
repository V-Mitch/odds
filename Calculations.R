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
  df_averages <- list()
  
  weights <- switch(weighting,
                    "normal" = rep(1, length.out = fixture_lookback),
                    "linear" = seq(1, 0, length.out = fixture_lookback),
                    "exponential" = exp(seq(-1, -fixture_lookback, 
                                            length.out = fixture_lookback)),
                    "soft" = exp(seq(-1, -fixture_lookback, 
                                     length.out = fixture_lookback))^0.25 /
                      sum(exp(seq(-1, -fixture_lookback, 
                                  length.out = fixture_lookback))^0.25)
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
  # for (i in 250:270){
  for (i in 1:nrow(fixture_dataframe)){
    remaining_fixtures <- fixture_dataframe[i:nrow(fixture_dataframe),]
    current_fixture <- remaining_fixtures %>%
      dplyr::slice(1)
    home_team <- current_fixture["teams.home.name"]
    away_team <- current_fixture["teams.away.name"]
    home_prior_5 <- remaining_fixtures %>%
      filter(teams.home.name == as.character(home_team) | 
               teams.away.name == as.character(home_team)) %>%
      dplyr::slice(-1) %>% 
      dplyr::slice_head(n = fixture_lookback)
    
    away_prior_5 <- remaining_fixtures %>%
      filter(teams.home.name == as.character(away_team) | 
               teams.away.name == as.character(away_team)) %>%
      dplyr::slice(-1) %>% 
      dplyr::slice_head(n = fixture_lookback)
    
    # only select when current home team was playing in the past as:
    # home and away team (swap the latter so calculations work later)
    home_prior_as_away  <- home_prior_5 %>%
      filter(teams.away.name == as.character(home_team)) %>% 
      swap_column_names()
    home_prior_as_home <- home_prior_5 %>%
      filter(teams.home.name == as.character(home_team))
    home_prior_5 <- bind_rows(home_prior_as_home, home_prior_as_away) %>% 
      arrange(desc(fixture.date))
    # same for current away team
    away_prior_as_away  <- away_prior_5 %>%
      filter(teams.away.name == as.character(away_team)) %>% 
      swap_column_names()
    away_prior_as_home <- away_prior_5 %>%
      filter(teams.home.name == as.character(away_team))
    away_prior_5 <- bind_rows(away_prior_as_home, away_prior_as_away) %>% 
      arrange(desc(fixture.date))
    
    # in case of empty past 5 matches, ensure theres a NaN placeholder
    if(nrow(home_prior_5) == 0){
      home_prior_5 <- home_prior_5 %>% add_row() %>% 
        mutate(across(everything(), as.numeric))
    } 
    if(nrow(away_prior_5) == 0){
      away_prior_5 <- away_prior_5 %>% add_row() %>% 
        mutate(across(everything(), as.numeric))
    } 
    
    # 
    for (j in seq_along(unique_vars_t1)) {
      var_temp <- unique_vars_t1[j]
      home_prior_5 <- home_prior_5 %>%
        mutate(!!diff_vars_t1[j] := .data[[paste0("team.1.", var_temp)]] - 
                 .data[[paste0("team.2.", var_temp)]])
      away_prior_5 <- away_prior_5 %>%
        mutate(!!diff_vars_t1[j] := .data[[paste0("team.1.", var_temp)]] - 
                 .data[[paste0("team.2.", var_temp)]])
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
      weights_temp <- weights[1:nrow(home_prior_5)] / sum(weights[1:nrow(home_prior_5)])
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
      weights_temp <- weights[1:nrow(away_prior_5)] / sum(weights[1:nrow(away_prior_5)])
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
    
    corenames <- colnames(avgs_for_home_team)
    colnames(avgs_for_away_team) <- paste0("away",corenames)                                       
    current_teams_differences <- avgs_for_home_team - avgs_for_away_team
    colnames(current_teams_differences) <- paste0("crtteams.diff.", 
                                                  corenames)
    
    df_averages[[i]] <- c(avgs_for_home_team, avgs_for_away_team, current_teams_differences, current_fixture$fixture.id)
    print(paste0("Calculated averages for fixture ",i,"/",nrow(fixture_dataframe)))
    
  }
  
  
  df_averages <- do.call(rbind, df_averages)
  
  rownames(df_averages) <- NULL
  df_averages <- as.data.frame(lapply(as.data.frame(df_averages), function(x) sapply(x, unlist)))
  colnames(df_averages) <- c(paste0(colnames(avgs_for_home_team)),
                             paste0(colnames(avgs_for_away_team)),
                             paste0(colnames(current_teams_differences)),
                             "fixture.id")
  
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


calculate_player_homogeneity <- function(fixture_dataframe, 
                                         post_game_varnames, 
                                         fixture_lookback = 5,
                                         weighting = "normal"){
  
  # arrange by date chronologically youngest to oldest game
  fixture_dataframe <- fixture_dataframe %>%
    arrange(desc(fixture.date))
  df_scores <- list()
  
  weights <- switch(weighting,
                    "normal" = rep(1, length.out = fixture_lookback),
                    "linear" = seq(1, 0, length.out = fixture_lookback),
                    "exponential" = exp(seq(-1, -fixture_lookback, 
                                            length.out = fixture_lookback)),
                    "soft" = exp(seq(-1, -fixture_lookback, 
                                     length.out = fixture_lookback))^0.25 /
                      sum(exp(seq(-1, -fixture_lookback, 
                                  length.out = fixture_lookback))^0.25)
  )
  # the player names
  relevant_variables <- fixture_dataframe[,grepl("player.name", names(fixture_dataframe))] %>% names()
  # other important variables
  other_variables <- c("teams.home.name", "teams.away.name", "fixture.date", "fixture.id")
  # for (i in 250:270){
  fixture_dataframe <- fixture_dataframe[,c(relevant_variables, other_variables)]
  
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
      dplyr::slice(1)
    home_team <- current_fixture["teams.home.name"]
    away_team <- current_fixture["teams.away.name"]
    home_prior_5 <- remaining_fixtures %>%
      filter(teams.home.name == as.character(home_team) | 
               teams.away.name == as.character(home_team)) %>%
      dplyr::slice_head(n = fixture_lookback+1)
    
    away_prior_5 <- remaining_fixtures %>%
      filter(teams.home.name == as.character(away_team) | 
               teams.away.name == as.character(away_team)) %>%
      dplyr::slice_head(n = fixture_lookback+1)
    
    # only select when current home team was playing in the past as:
    # home and away team (swap the latter so calculations work later)
    home_prior_as_away  <- home_prior_5 %>%
      filter(teams.away.name == as.character(home_team)) %>% 
      swap_column_names()
    home_prior_as_home <- home_prior_5 %>%
      filter(teams.home.name == as.character(home_team))
    home_prior_5 <- bind_rows(home_prior_as_home, home_prior_as_away) %>% 
      arrange(desc(fixture.date))
    # same for current away team
    away_prior_as_away  <- away_prior_5 %>%
      filter(teams.away.name == as.character(away_team)) %>% 
      swap_column_names()
    away_prior_as_home <- away_prior_5 %>%
      filter(teams.home.name == as.character(away_team))
    away_prior_5 <- bind_rows(away_prior_as_home, away_prior_as_away) %>% 
      arrange(desc(fixture.date))
    
    # function to obtain get the player list per game neatly
    player_names_per_game <- function(prior_games, substring_category){
      lists_names <- prior_games %>%
        select(matches(substring_category)) %>%  # Select columns containing the substring selected
        rowwise() %>%                  # Operate row-wise
        mutate(unique_values = list(unique(c_across(everything())))) %>%  # Create a list of unique values
        pull(unique_values)  
      return(lists_names)
    }
    
    # calculate the overall presence of players for a given list (overall, starting players, or substitutes) 
    calculate_player_presence <- function(name_list, fixture_lookback){
      # Flatten the list into a single vector while removing NA values
      all_names <- unlist(name_list)
      all_names <- all_names[!is.na(all_names)]
      # Count occurrences of each name across all vectors, normalize for game amounts
      name_counts_normalized <- table(all_names)/(fixture_lookback+1)
      return(name_counts_normalized)
    }
    
    obtain_overall_score <- function(prior_games, fixture_lookback){
      # use the established smaller functions above to calculate presence of players over the time-span
      lists_names <- player_names_per_game(prior_games, "team.1")
      startXI_names <- player_names_per_game(prior_games, "startXI.team.1")
      substitutes_names <- player_names_per_game(prior_games, "substitutes.team.1")
      all_player_presence <- calculate_player_presence(lists_names, fixture_lookback+1)
      startXI_presence <- calculate_player_presence(startXI_names, fixture_lookback+1)
      substitutes_presence <- calculate_player_presence(substitutes_names, fixture_lookback+1)
      # Calculate the similarity score
      # In this case, we can calculate the sum of squared counts
      similarity_score_all_players <- sum(all_player_presence^2)/length(all_player_presence)
      similarity_score_startXI <- sum(startXI_presence^2)/length(startXI_presence)
      similarity_score_substitutes <- sum(substitutes_presence^2)/length(substitutes_presence)
      # place more weight for the overall similarity
      overall_score <- (3*similarity_score_all_players + similarity_score_startXI + similarity_score_substitutes)/5
      return(overall_score)
    }
    
    overall_score_home <- if_else(nrow(home_prior_5) == (fixture_lookback+1),obtain_overall_score(home_prior_5, fixture_lookback),NA)
    overall_score_away <- if_else(nrow(away_prior_5) == (fixture_lookback+1),obtain_overall_score(away_prior_5, fixture_lookback),NA)
    
    df_scores[[i]] <- c(overall_score_home, overall_score_away, current_fixture$fixture.id)
    print(paste0("Calculated homogeneity scores for fixture ",i,"/",nrow(fixture_dataframe)))
    browser()
  }
  
  
  df_scores <- do.call(rbind, df_scores)
  # browser()
  rownames(df_scores) <- NULL
  df_scores <- as.data.frame(lapply(as.data.frame(df_scores), function(x) sapply(x, unlist)))
  colnames(df_scores) <- c("teams.home.homogeneity", "teams.away.homogeneity", "fixture.id")
  
  return(df_scores)
}