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