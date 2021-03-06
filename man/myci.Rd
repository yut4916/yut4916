% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myci.R
\name{myci}
\alias{myci}
\title{Confidence Interval}
\usage{
myci(x)
}
\arguments{
\item{x}{vector; data values to construction interval around}
}
\value{
list containing the lower and upper bounds
}
\description{
Calculates a 95 percent confidence interval for the mean of the data provided.
}
\examples{
x=rnorm(1:100); myci(x)
}
