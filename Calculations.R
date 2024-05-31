# Custom Functions

fairing_odds_ <- function(odds1, odds2, odds3){
  markup <- sum(1/odds1, 1/odds2, 1/odds3) - 1
  fair1 <- odds1*(1+markup)
  fair2 <- odds2*(1+markup)
  fair3 <- odds3*(1+markup)
  return(list(fair1,fair2,fair3, markup))
}

fairing_odds <- function(...){
  args <- list(...)
  total_sum <- 0
  for (arg in args){
      total_sum <- (total_sum + 1/arg)
    }
    markup <- total_sum - 1
    fair_list <- c(rep(NaN,length(args)))
  for (i in 1:length(fair_list)){
    # browser()
    fair_list[i] <- (args[[i]]*(1+markup))
  }
    return(fair_list)
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