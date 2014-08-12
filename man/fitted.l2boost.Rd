\name{fitted.l2boost}
\alias{fitted.l2boost}
\title{Extract the fitted model estimates along the solution path for an l2boost model.}
\usage{
  \method{fitted}{l2boost} (object, m = NULL, ...)
}
\arguments{
  \item{object}{an l2boost object}

  \item{m}{the iteration number with the l2boost path.
  (defualt m=NULL)}

  \item{...}{other arguments}
}
\value{
  The vector of fitted response estimates at the given
  iteration m. By default, the coefficients are obtained
  from the last iteration m=M.
}
\description{
  Extract the fitted model estimates along the solution
  path for an l2boost model.
}
\details{
  \code{\link{fitted}} is a generic function which extracts
  fitted values from objects returned by modeling
  functions.

  \code{\link{fitted.l2boost}} returns the function
  estimates obtained from the training set observations of
  an l2boost model object at any point along the solution
  path. The estimate, F_m(x) is evaluated at iteration m
  using the training data set x. By default,
  \code{\link{fitted.l2boost}} returns the estimate at the
  last iteration step M, unless a specific iteration step m
  is specified.
}
\examples{
#--------------------------------------------------------------------------
# Example: Diabetes
#
# See Efron B., Hastie T., Johnstone I., and Tibshirani R.
# Least angle regression. Ann. Statist., 32:407-499, 2004.
data(diabetes, package="l2boost")

l2.object <- l2boost(diabetes$x,diabetes$y, M=1000, nu=.01)

# return the fitted values
fitted(l2.object)
fitted(l2.object, m=500)

#' # Create diagnostic plots
par(mfrow=c(2,2))
qqnorm(fitted(l2.object), ylim=c(0, 300))
qqline(fitted(l2.object), col=2)

qqnorm(fitted(l2.object, m=500), ylim=c(0, 300))
qqline(fitted(l2.object, m=500), col=2)

# Tukey-Anscombe's plot
plot(y=residuals(l2.object), x=fitted(l2.object), main="Tukey-Anscombe's plot",
  ylim=c(-3e-13, 3e-13))
lines(smooth.spline(fitted(l2.object), residuals(l2.object), df=4), type="l",
  lty=2, col="red", lwd=2)
abline(h=0, lty=2, col = 'gray')

plot(y=residuals(l2.object, m=500), x=fitted(l2.object, m=500),
  main="Tukey-Anscombe's plot", ylim=c(-3e-13, 3e-13))
lines(smooth.spline(fitted(l2.object,m=500), residuals(l2.object, m=500), df=4),
  type="l", lty=2, col="red", lwd=2)
abline(h=0, lty=2, col = 'gray')
}
\seealso{
  \code{\link{fitted}} and \code{\link{l2boost}} and
  \code{\link{predict.l2boost}}
}

