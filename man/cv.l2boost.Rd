\name{cv.l2boost}
\alias{cv.l2boost}
\title{cv.l2boost K fold Cross Validation of the l2boost algorithm}
\usage{
  cv.l2boost(x, y, K = 10, M = NULL, nu = 1e-04,
    lambda = NULL, trace = FALSE,
    type = c("friedman", "discrete", "hybrid", "lars"),
    ...)
}
\arguments{
  \item{K}{= 10 number of cross validation folds}
}
\description{
  cv.l2boost K fold Cross Validation of the l2boost
  algorithm
}

