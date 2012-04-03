\name{fitted.l2boost}
\alias{fitted.l2boost}
\title{fitted.l2boost returns the function F_m(x) evaluated at
iteration m using the training data set x}
\usage{
  fitted.l2boost(object, m = NULL, ...)
}
\arguments{
  \item{object}{an l2boost object for the extraction of
  model coefficients.}

  \item{m}{the iteration number with the l2boost path. If
  m=NULL, the coefficients are obtained from the last
  iteration M.}

  \item{...}{other arguments}
}
\description{
  fitted.l2boost returns the function F_m(x) evaluated at
  iteration m using the training data set x
}

