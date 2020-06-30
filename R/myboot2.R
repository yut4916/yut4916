#' Bootstrap Sampling
#'
#' @param iter number; the number of samples to take from the data provided
#' @param x vector; a vector containing the data to sample from
#' @param fun string; the function name to apply across each sample, calculating the sample statistic
#' @param alpha number; a number between 0 and 1 indicating the confidence interval
#' @param cx number; graphical parameter
#' @param ... additional graphing parameters for hist()
#'
#' @return list containg the confidence interval, function, data, and histogram parameters
#' @export
#'
#' @examples data=rnorm(1:100); myboot2(x = data, alpha=0.05, iter=10000, fun="mean", col="red")
myboot2 <- function(iter=10000, x, fun="mean", alpha=0.05, cx=1.5, ...){
  n <- length(x) # sample size

  y <- sample(x, n*iter, replace=TRUE) # A
  rs.mat <- matrix(y, nr=n, nc=iter, byrow=TRUE)
  xstat <- apply(rs.mat, 2, fun) # a vector with iter values in it
  ci <- quantile(xstat, c(alpha/2, 1-alpha/2)) # nice way to form a confidence interval # B

  # contains histogram parameters
  para <- hist(xstat,freq=FALSE,las=1,
               main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
               ...)

  # matrix contains the data, allows us to use apply()
  mat <- matrix(x, nr=length(x), nc=1, byrow=TRUE)

  # pte = point estimate
  pte <- apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black") # vertical line
  segments(ci[1],0,ci[2],0,lwd=4) # make segment for ci
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)

  # plot the point estimate halfway up the density
  text(pte,max(para$density)/2,round(pte,2),cex=cx)

  invisible(list(ci=ci,fun=fun,x=x,param=para)) # output to use if necessary
}
