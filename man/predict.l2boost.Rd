\name{predict.l2boost}
\alias{predict.l2boost}
\title{predict method for l2boost objects.}
\usage{
  \method{predict}{l2boost} (object, xnew = NULL,
    type = c("fit", "coef"), newdata = xnew, ...)
}
\arguments{
  \item{object}{an l2boost objectect}

  \item{xnew}{a new design matrix to fit with the l2boost
  object}

  \item{newdata}{a new design matrix to fit with the
  l2boost object}

  \item{type}{fit returns the predicted values coef returns
  the model coefficients}

  \item{...}{other arguments (currently not used)}
}
\value{
  function estimates for type=fit, coefficient estimates
  for type=coef \item{yhat}{vector of n function estimates
  from the final step M} \item{yhat.path}{list of M
  function estimates at each step m}

  or

  \item{coef}{vector of p coefficient estimates from final
  step M} \item{coef.stand}{vector of p standardized
  coefficient estimates from final step M}
  \item{coef.path}{list of vectors of p coefficient
  estimates, one for each step m}
  \item{coef.stand.path}{list of vectors of p standardized
  coefficient estimates, one for each step m}
}
\description{
  predict method for l2boost objects.
}
\examples{
#--------------------------------------------------------------------------
# Example 1: Diabetes
#
# See Efron B., Hastie T., Johnstone I., and Tibshirani R.
# Least angle regression. Ann. Statist., 32:407-499, 2004.
data(diabetes)

object <- l2boost(diabetes$x,diabetes$y, M=1000, nu=.01)

# With no arguments returns the estimates from the training data.
predict(object)

# Also can return coefficient estimates
predict(object, type="coef")

# Or used to predict new data, in this case a subset of training data
cbind(diabetes$y[1:5], predict(object, xnew=diabetes$x[1:5,]))
}
\seealso{
  \code{\link{predict}} and \code{link{l2boost}}
}

