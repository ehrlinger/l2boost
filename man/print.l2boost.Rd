\name{print.l2boost}
\alias{print.l2boost}
\title{print method for \code{\link{l2boost}} and \code{\link{cv.l2boost}} objects.}
\usage{
  \method{print}{l2boost} (x, m = NULL, ...)
}
\arguments{
  \item{x}{an l2boost object}

  \item{m}{return the result from iteration m}

  \item{...}{other arguments passed to helper functions}
}
\description{
  \code{\link{print}} is a generic function for displaying
  model summaries

  \code{\link{print.l2boost}} returns a model summary for
  \code{\link{l2boost}} and \code{\link{cv.l2boost}}
  objects including the coefficient estimates at the
  specified step m. By default, \code{\link{print.l2boost}}
  returns the summary for the object at the final iteration
  step M
}
\examples{
#--------------------------------------------------------------------------
# Example 1: Diabetes
#
# See Efron B., Hastie T., Johnstone I., and Tibshirani R.
# Least angle regression. Ann. Statist., 32:407-499, 2004.
data(diabetes)

object <- l2boost(diabetes$x,diabetes$y, M=1000, nu=.01)

# A summary of the l2boost object at M=1000
print(object)

# Similar at m=100
print(object, m=100)
}
\seealso{
  \code{\link{l2boost}}, \code{\link{cv.l2boost}} and
  \code{\link{coef.l2boost}}
}

