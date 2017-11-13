#' Calculate gini coefficient
#'
#'  This functions calculates gini coefficient, with or without weights
#'  @param x input data
#'  @param weights corresponding weights to data, default weight is 1 for each data point
#'  @return Gini Coefficient
#'
#'  @export

gini <- function (x, weights = rep(1,length(x))){
  x <- sort(rep(as.numeric(x),weights))
  n <- length(x)
  out <- n*(2*sum(x*(1:n))/(n*sum(x))-(1/n)-1)/(n-1)
  return(pmax(0,out))
}

#' Calculate palma ratio
#'
#'  This functions calculates palma ratio, with or without weights, with customizable ranges for top and bottom income ranges
#'  @param x input data
#'  @param weights corresponding weights to data, default weight is 1 for each data point
#'  @param bot percentage of poorest population used, default 40
#'  @param top percentage of richest population used, default 10
#'  @return Palma Ratio
#'
#'  @export
palma <- function(x,weights = rep(1,length(x)),bot = 40,top = 10){
  x <- sort(rep(as.numeric(x),weights))
  n <- length(x)
  bot <- (bot/100)*n
  top <- (top/100)*n
  botsum <- (x[1:bot])
  topsum <- (x[-(1:(n-top+1))])
  return(mean(topsum)/mean(botsum))
}

#' Calculate Theil's T Coefficent
#'
#'  This functions calculates Theil's T coefficient, with or without weights
#'  @param x input data
#'  @param weights corresponding weights to data, default weight is 1 for each data point
#'  @return Theil's T COefficient
#'
#'  @export
TheilsT <- function(x,weights = rep(1,length(x))){
  n <- length(x)
  mean <- mean(x)
  wsum <- sum(weights)
  data <- x/mean
  data <- weights/wsum*data*log(data)
  sum(data)
}
