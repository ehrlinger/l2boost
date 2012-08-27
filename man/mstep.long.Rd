\name{mstep.long}
\alias{mstep.long}
\title{l2boost function to determine step size for long descent from Theorem 2 of Ehrlinger and Ishwaran (2012)
|Dk| must be less than or equal to 1; We force it if otherwise, to enforce step=infty when dk=Rk=1}
\usage{
  mstep.long(rho.m, Rk, lr, nu)
}
\arguments{
  \item{rho.m}{vector of stagewise regression parameters}

  \item{Rk}{correlation coefficient of candidate and
  current directions}

  \item{lr}{current direction index}

  \item{nu}{l1 shrinkage parameter}
}
\description{
  l2boost function to determine step size for long descent
  from Theorem 2 of Ehrlinger and Ishwaran (2012) |Dk| must
  be less than or equal to 1; We force it if otherwise, to
  enforce step=infty when dk=Rk=1
}
\references{
  John Ehrlinger, Hemant Ishwaran (2012). Characterizing
  l2boosting. \emph{Annals of Statistics}, 40 (2),
  1074-1101
}

