\name{nu.limit}
\alias{nu.limit}
\title{limiting nu size, this comes directly from the RHS of theorem 2 of Ehrlinger and Ishwaran 2012.}
\usage{
  nu.limit(rho.m, Rk, lr)
}
\arguments{
  \item{rho.m}{vector of stagewise regression parameters}

  \item{Rk}{correlation coefficient of current and
  candidate coordinate directions}

  \item{lr}{candidate direction index}
}
\description{
  limiting nu size, this comes directly from the RHS of
  theorem 2 of Ehrlinger and Ishwaran 2012.
}
\references{
  Ehrlinger and Ishwaran (2012). Characterizing l2boosting.
  \emph{Annals of Statistics}, 40 (2), 1074-1101
}

