
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



