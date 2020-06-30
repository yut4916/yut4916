#' Poisson CLT
#'
#' @param n number; sample size
#' @param iter number; number of samples to draw
#' @param lambda number; lambda parameter for the Poisson distribution
#' @param ... histogram plotting parameters
#'
#' @return series of three plots
#' @export
#'
#' @examples mycltp(30, 10000)
mycltp <- function(n, iter, lambda=10,...){
  # random sample from the Poisson
  y <- rpois(n*iter, lambda=lambda)

  # place sample into matrix
  # columns correspond to iteration, rows equal sample size n
  data <- matrix(y, nr=n, nc=iter, byrow=TRUE)

  # apply the function mean to the columns (2) of the matrix and place in vector w
  w <- apply(data, 2, mean)

  # all values used to make a histogram are placed in param (nothing is plotted yet)
  param <- hist(w,plot=FALSE)

  # find the max density, add 10%
  ymax <- 1.1*max(param$density)

  # make a suitable layout for graphing
  layout(matrix(c(1,1,2,3), nr=2, nc=2, byrow=TRUE))

  # make the histogram
  hist(w,freq=FALSE,  ylim=c(0,ymax), col=rainbow(max(w)),
       main=paste("Histogram of sample mean","\n", "sample size= ",n," iter=",iter," lambda=",lambda,sep=""),
       xlab="Sample mean",...)

  # add a density curve made from the sample distribution
  lines(density(w),col="Blue",lwd=3)

  # add a theoretical normal curve
  curve(dnorm(x,mean=lambda,sd=sqrt(lambda/n)),add=TRUE,col="Red",lty=2,lwd=3)

  # make a new plot
  # since y is discrete we should use a barplot
  barplot(table(y)/(n*iter),col=rainbow(max(y)), main="Barplot of sampled y", ylab ="Rel. Freq",xlab="y" )
  x <- 0:max(y)
  plot(x,dpois(x,lambda=lambda),type="h",lwd=5,col=rainbow(max(y)),
       main="Probability function for Poisson", ylab="Probability",xlab="y")
}
