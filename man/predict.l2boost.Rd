\name{predict.l2boost}
\alias{predict.l2boost}
\title{predict method for l2boost models.}
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

  \item{type}{"fit" or "coef" determins the values
  returned. "fit" returns model estimates, "coef" returns
  the model coefficients}

  \item{...}{other arguments (currently not used)}
}
\value{
  function estimates for type=fit, coefficient estimates
  for type=coef \itemize{ \item{yhat}{vector of n function
  estimates from the final step M} \item{yhat.path}{list of
  M function estimates, one at each step m} } or \itemize{
  \item{coef}{vector of p beta coefficient estimates from
  final step M} \item{coef.stand}{vector of p standardized
  beta coefficient estimates from final step M}
  \item{coef.path}{list of vectors of p beta coefficient
  estimates, one for each step m}
  \item{coef.stand.path}{list of vectors of p standardized
  beta coefficient estimates, one for each step m} }
}
\description{
  \code{\link{predict}} is a generic function for
  predictions from the results of various model fitting
  functions.
}
\details{
  \code{\link{predict.l2boost}} takes the optional
  \emph{xnew} (equivalent \emph{newdata})
  \code{\link{data.frame}} and returns the model estimates
  from an \code{\link{l2boost}} object. If neither
  \emph{xnew} or \emph{newdata} are provided,
  \code{\link{predict}} returns estimates for the
  \code{\link{l2boost}} training data set.

  By default, \code{\link{predict.l2boost}} returns the
  function estimates, unless type="coef" then the set of
  regression coefficients (beta) are returned from the
  \code{\link{l2boost}} object.
}
\examples{
#--------------------------------------------------------------------------
# Example 1: Diabetes
#
# See Efron B., Hastie T., Johnstone I., and Tibshirani R.
# Least angle regression. Ann. Statist., 32:407-499, 2004.
data(diabetes)

object <- l2boost(diabetes$x,diabetes$y, M=1000, nu=.01)

# With no arguments returns the estimates at the full M from the training data.
prd <- predict(object)
prd$yhat

# at step m=600
prd$yhat.path[[600]]

# Also can return coefficient estimates. This is equivalent to \\code{\\link{coef.l2boost}}
cf <- predict(object, type="coef")
cf$coef

# at step m=600
cf$coef.path[[600]]

# Or used to predict new data, in this case a subset of training data
cbind(diabetes$y[1:5], predict(object, xnew=diabetes$x[1:5,])$yhat)
}
\seealso{
  \code{\link{predict}} and \code{\link{l2boost}},
  \code{\link{coef.l2boost}}, \code{\link{fitted.l2boost}},
  \code{\link{residuals.l2boost}} and
  \code{\link{cv.l2boost}}
}

