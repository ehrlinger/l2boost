\name{get.lars.solution}
\alias{get.lars.solution}
\title{l2boost internal method to get l2boost-lars limiting solution. This function is detailed in
section 4.7 of Ehrlinger 2011}
\usage{
  get.lars.solution(rho.m, corr.x, activeS, qr.tol,
    eps.tol)
}
\arguments{
  \item{rho.m}{vector of gradient corellations to this
  point (m)}

  \item{corr.x}{correlation matrix}

  \item{activeS}{Acrive set}

  \item{qr.tol}{the tolerance for detecting linear
  dependencies in the qr.solve}

  \item{eps.tol}{limiting value for dynamic boosting step
  size}
}
\description{
  l2boost internal method to get l2boost-lars limiting
  solution. This function is detailed in section 4.7 of
  Ehrlinger 2011
}
\references{
  Ehrlinger (2011). Regularization: Stagewise Regression
  and Bagging, Ph.D. Dissertation, Case Western Reserve
  University
}
\seealso{
  \code{\link{qr.solve}}
}

