#' Confidence Interval
#'
#' @description Calculates a 95 percent confidence interval for the mean of the data provided.
#'
#' @param x vector; data values to construction interval around
#'
#' @return list containing the lower and upper bounds
#' @export
#'
#' @examples x=rnorm(1:100); myci(x)
myci <- function(x){
  a <- 0.05
  n <- length(x)
  ybar <- mean(x)
  s <- sd(x)

  t <- qt(1 - a/2, n)

  lower <- ybar - t*s/sqrt(n)
  upper <- ybar + t*s/sqrt(n)
  list(Lower = lower, Upper = upper)
}
