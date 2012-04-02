\name{l2boost}
\alias{l2boost}
\alias{l2boost.formula}
\title{l2boost: linear regression boosting with an l2 loss function.}
\usage{
  \method{l2boost}{default} (x, y, M = NULL, nu = 1e-04,
    lambda = NULL, trace = FALSE,
    type = c("friedman", "discrete", "hybrid", "lars"),
    qr.tolerance = 1e-30,
    eps.tolerance = .Machine$double.eps, ...)

  l2boost.formula(formula, data = list(), ...)
}
\arguments{
  \item{x}{design matrix of dimension n x p}

  \item{y}{response variable of length n}

  \item{formula}{an object of class \code{\link{formula}}
  (or one that can be coerced to that class): a symbolic
  description of the model to be fitted. The details of
  model specification are given under "Details".}

  \item{data}{an optional data frame, list or environment
  (or object coercible by \code{\link{as.data.frame}} to a
  data frame) containing the variables in the model.  If
  not found in data, the variables are taken from
  "environment(formula)", typically the environment from
  which \code{\link{lm}} is called.}

  \item{M}{number of steps to run boost algorithm (M >1)}

  \item{nu}{l1 shrinkage parameter (0 < nu <= 1)}

  \item{lambda}{l2 shrinkage parameter used for elastic net
  boosting (lambda > 0 || lambda = NULL)}

  \item{type}{Choice of l2boost algorithm from "friedman",
  "discrete", "hybrid", "lars"}

  \item{qr.tolerance}{tolerance limit for use in
  \code{\link{qr.solve}} (default: 1e-30)}

  \item{eps.tolerance}{dynamic step size lower limit
  (default: .Machine$double.eps)}

  \item{trace}{show runtime messages (default: FALSE)}

  \item{...}{other arguments (currently unused)}
}
\value{
  A "l2boost" object is returned, for which print, plot,
  predict, and coef methods exist.
}
\description{
  l2boost implements the boosting using an l2-loss function
}
\details{
  l2boost is a fast implementation of Friedman's boosting
  algorithm with coordinate direction base learners and an
  l2-loss function.
}
\examples{
data(diabetes)
    par(mfrow=c(2,2))
    attach(diabetes)
    object <- l2boost(x,y, M=1000, nu=.01)
    plot(object)
    plot(object, type="coef")

    object2 <- l2boost(x,y,M=10000, nu=1.e-3) # increased shrinkage and number of iterations.
    plot(object2)
    plot(object2, type="coef")


    dta <- mvnorm.l2boost()
    object3 <- l2boost(x,y,M=10000, nu=1.e-3, lambda=.05) # elasticNet Boosting with l2 shrinkage
    plot(object3)
    object4 <- l2boost(x,y,M=10000, nu=1.e-3, lambda=.1) # elasticNet Boosting with more l2 shrinkage
    plot(object4)
    detach(diabetes)
}
\references{
  John Ehrlinger, Hemant Ishwaran (2012). Characterizing
  l2boosting. \emph{Annals of Statistics}, to appear.
}
\seealso{
  \code{\link{print.l2boost}}, \code{\link{plot.l2boost}},
  \code{\link{predict.l2boost}} methods of l2boost and
  \code{\link{cv.l2boost}}
}

