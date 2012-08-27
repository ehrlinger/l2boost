\name{fitted.l2boost}
\alias{fitted.l2boost}
\title{returns the fitted function values of an l2boost model object, F_m(x) evaluated at
iteration m using the training data set x}
\usage{
  \method{fitted}{l2boost} (object, m = NULL, ...)
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
  returns the fitted function values of an l2boost model
  object, F_m(x) evaluated at iteration m using the
  training data set x
}
\examples{
\dontrun{
#--------------------------------------------------------------------------
# Example 1: Diabetes
#
# See Efron B., Hastie T., Johnstone I., and Tibshirani R.
# Least angle regression. Ann. Statist., 32:407-499, 2004.
data(diabetes)

object <- l2boost(diabetes$x,diabetes$y, M=1000, nu=.01)
fitted(object)
}
}
\seealso{
  \code{\link{fitted}} and \code{link{l2boost}}
}

