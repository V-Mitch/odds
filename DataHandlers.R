
# Function to unlist bookmakers and extract the odds
extract_odds <- function(row) {
  output_df <- data.frame(matrix(ncol = 0, nrow = 1))
  bkmkr <- row$bookmakers[[1]]
  bookmaker_name <- bkmkr["key"]
  mrkt <- bkmkr["markets"][[1]][[1]]
  for (i in 1:length(mrkt$outcomes)) {
    outcome_name <- paste("outcome_",i, sep = "")
    price_name <- paste("price_",i, sep = "")
    temp_frame <- data.frame(n1 = mrkt$outcomes[[i]]$name, n2 = mrkt$outcomes[[i]]$price)
    output_df <- cbind(output_df, setNames(temp_frame, c(outcome_name, price_name)))
  }
  meta_vars <- data.frame(
    id = row$id,
    commence_time = row$commence_time,
    bookmaker = bookmaker_name,
    home_team = row$home_team,
    away_team = row$away_team
  )
  output_df <- cbind(meta_vars, output_df)
  return(output_df)
}

extract_over_row <- function(data){
  final_df <- data.frame()
  for (i in 1:dim(data)[1]) {
    temp_df <- extract_odds(data[i])
    final_df <- rbind(temp_df, final_df)
  }
  return(final_df)
}

batch_from_api <- function(leagues = 10, 
                           seasons = 2024, 
                           api_key = api_key, 
                           api_host = api_host, 
                           short_limit = 300){
  for (i in 1:length(leagues)){
    for(j in 1:length(seasons)){
      temp_matches <- get_past_matches(leagues = leagues[i], 
                                       seasons = seasons[j], 
                                       api_key = api_key, 
                                       api_host = api_host, 
                                       short_limit = short_limit)
    }
  }
  namefile <- paste0(Sys.Date(), "__", seasons,"_", leagues)
  save(temp_matches, file = namefile)
  Sys.sleep(60 + 1)
}

load_and_assign <- function(file, varname) {
  load(file)
  obj_name <- ls()[1]  # Get the name of the loaded object
  assign(varname, get(obj_name), envir = .GlobalEnv)
  rm(list = obj_name)  # Remove the object from the global environment
}