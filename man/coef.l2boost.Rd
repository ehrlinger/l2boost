\name{coef.l2boost}
\alias{coef.l2boost}
\title{coef method for l2boost objects extracts model coefficients from an l2boost fit object.}
\usage{
  \method{coef}{l2boost} (object, m = NULL, ...)
}
\arguments{
  \item{object}{an l2boost fit object}

  \item{m}{the iteration number within the l2boost path. If
  m=NULL, the coefficients are obtained from the last
  iteration M.}

  \item{...}{generic arguments passed to parent function}
}
\value{
  the un-standardized coefficient estimates for l2boost
  objects. Ov objrct is a cv.l2boost, return the
  unstandardized coefficients at the opt.step of the full
  model.
}
\description{
  coef method for l2boost objects extracts model
  coefficients from an l2boost fit object.
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
coef(object)
}
}
\seealso{
  \code{\link{coef}} and
  \code{\link{l2boost}},\code{\link{cv.l2boost}},
  \code{\link{predict.l2boost}} methods of l2boost
}

