% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/knapsack_dynamic.R
\name{knapsack_dynamic}
\alias{knapsack_dynamic}
\title{knapsack_dynamic}
\usage{
knapsack_dynamic(x, W)
}
\arguments{
\item{x}{data frame with two variables v and w}

\item{W}{knapsack size}
}
\value{
list with value and elements
}
\description{
Implementation of a dynamic programming algorithm for the knapsack problem,
taken from this page: \url{https://en.wikipedia.org/wiki/Knapsack_problem}
}
\examples{
RNGversion(min(as.character(getRversion()),"3.5.3"))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
data.frame(
   w=sample(1:4000, size = n, replace = TRUE),
   v=runif(n = n, 0, 10000)
 )
x <- knapsack_objects[1:8,]
knapsack_dynamic(x = x, W = 3500)
}
