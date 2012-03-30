\name{get.lars.solution}
\alias{get.lars.solution}
\title{l2boost internal method to get l2boost-lars limiting solution}
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
  solution
}
\seealso{
  \code{\link{qr.solve}}
}

