% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multi.R
\name{multi}
\alias{multi}
\title{Execute an Expression Multiple Times}
\usage{
multi(expr, times = 10)
}
\arguments{
\item{expr}{An R expression to be evaluated. It should be a valid R code without quotes.}

\item{times}{An integer indicating how many times to execute the expression. Default is 10.}
}
\value{
A list containing the results of each execution of the expression.
}
\description{
This function takes an R expression and executes it a specified number of times.
}
\examples{
multi(sample(x = 1:10, size = 2))

multi({
  P <- sample(x = 1:10, size = 2)
  mean(P)
}, times = 5)

}
