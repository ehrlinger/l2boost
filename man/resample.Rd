\name{resample}
\alias{resample}
\title{robust resample method allows resampling when length(x) <= 1. This is an
implementation suggested by the \code{\link{sample}} function in Base R.}
\usage{
  resample(x, size = 1, ...)
}
\arguments{
  \item{x}{a vector of one or more elements from which to
  choose}

  \item{size}{a non-negative integer giving the number of
  items to choose}

  \item{...}{other arguments passed to
  \code{\link{sample}}}
}
\description{
  robust resample method allows resampling when length(x)
  <= 1. This is an implementation suggested by the
  \code{\link{sample}} function in Base R.
}
\seealso{
  \code{\link{sample}}
}

