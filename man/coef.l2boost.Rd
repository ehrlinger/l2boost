\name{coef.l2boost}
\alias{coef.l2boost}
\title{Extract model coefficients from an l2boost model object.}
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
  vector of coefficient estimates for \code{\link{l2boost}}
  objects. The estimates correspond to the given iteration
  \emph{m}.
}
\description{
  \code{\link{coef}} is a generic function which extracts
  model coefficients from objects returned by modeling
  functions.

  By default, \code{\link{coef.l2boost}} returns the model
  (beta) coefficients from the last step, M of the
  \code{\link{l2boost}} model. For a
  \code{\link{cv.l2boost}} object, the default returns the
  coefficients from full model, at the cross-validation
  optimal step (\emph{m = opt.step} return value).

  Coefficients from alternative steps can be obtained using
  the \emph{m} parameter.
}
\examples{
#--------------------------------------------------------------------------
# Example: Diabetes data
#
# See Efron B., Hastie T., Johnstone I., and Tibshirani R.
# Least angle regression. Ann. Statist., 32:407-499, 2004.
data(diabetes, package='l2boost')

object <- l2boost(diabetes$x,diabetes$y, M=1000, nu=.01)
coef(object)

# At the m=500 step
coef(object, m=500)
}
\seealso{
  \code{\link{coef}} and \code{\link{l2boost}},
  \code{\link{cv.l2boost}} and
  \code{\link{predict.l2boost}} methods of l2boost
}

