\name{l2boost}
\alias{l2boost}
\alias{l2boost.default}
\alias{l2boost.formula}
\title{l2boost: linear regression boosting with an l2 loss function.}
\usage{
  l2boost(x, ...)

  l2boost.default(x, y, M, nu, lambda, trace, type,
    qr.tolerance, eps.tolerance, ...)

  l2boost.formula(formula, data, ...)
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
  "discrete", "hybrid", "lars" friedman - Original,
  bare-bones l2boost (Friedman (2001)) discrete - Optimized
  Friedman to reduce number of evaluations. dynamically
  determine number of steps to take along a descent
  direction. Discrete allows the algorithm to take step
  sizes of multiples of nu at any evaluation. Fastest
  option. hybrid - Similar to discrete, however only allows
  combining steps along the first descent direction. Works
  best if nu is moderate but not too small. lars - Get the
  l2boost-lars-limit (See Efron et.al (2004))}

  \item{qr.tolerance}{tolerance limit for use in
  \code{\link{qr.solve}} (default: 1e-30)}

  \item{eps.tolerance}{dynamic step size lower limit
  (default: .Machine$double.eps)}

  \item{trace}{show runtime messages (default: FALSE)}

  \item{...}{other arguments (currently unused)}
}
\value{
  A "l2boost" object is returned, for which print, plot,
  predict, and coef methods exist. \item{call}{the matched
  call.} \item{type}{Choice of l2boost algorithm from
  "friedman", "discrete", "hybrid", "lars"} \item{nu}{The
  l1 boosting shrinkage parameter value} \item{lambda}{The
  l2 elasticNet shrinkage parameter value} \item{x}{The
  training dataset} \item{x.na}{Columns of original design
  matrix with values na, these have been removed from x}
  \item{x.attr}{scale attributes of design matrix}
  \item{names}{Column names of design matrix}
  \item{y}{training response vector associated with x,
  centered about the mean value ybar} \item{ybar}{mean
  value of training response vector} \item{mjk}{measure to
  favorability. This is a matrix of size p by m. Each
  coordinate j has a measure at each step m}
  \item{stepSize}{} \item{l.crit}{vector of column index of
  critical direction} \item{L.crit}{number of steps along
  each l.crit direction} \item{S.crit}{The critical step
  value where a direction change occurs}
  \item{path.Fm}{estimates of response at each step m}
  \item{Fm}{estimate of response at final step M}
  \item{rhom.path}{boosting parameter estimate at each step
  m} \item{betam.path}{beta parameter estimates at each
  step m. List of m vectors of length p} \item{betam}{beta
  parameter estimate at final step M}
}
\description{
  l2boost is a fast implementation of Friedman's boosting
  algorithm with coordinate direction base learners and an
  l2-loss function.
}
\examples{
#--------------------------------------------------------------------------
# Example 1: Diabetes
#
# See Efron et al. (2004).
data(diabetes, package = "l2boost")

object <- l2boost(diabetes$x,diabetes$y, M=1000, nu=.01)

# Plot the boosting rho, and regression beta coefficients as a function of
# boosting steps m
par(mfrow=c(2,2))
plot(object)
plot(object, type="coef")

# increased shrinkage and number of iterations.
object2 <- l2boost(diabetes$x,diabetes$y,M=10000, nu=1.e-3)
plot(object2)
plot(object2, type="coef")


#--------------------------------------------------------------------------
# Example 2: ElasticBoost simulation
# Compare l2boost and elasticNetBoosting using 10-fold CV
#
# See Zou H. and Hastie T. Regularization and variable selection via the
# elastic net. J. Royal Statist. Soc. B, 67(2):301-320, 2005
dta <- elasticNetSim(n=100)

# l2boost the simulated data with groups of correlated coordinates
object3 <- l2boost(dta$x,dta$y,M=10000, nu=1.e-3, lambda=NULL)

# 10 fold l2boost cross validation to find "optimal" solution.
cv.obj3 <- cv.l2boost(dta$x,dta$y,M=10000, nu=1.e-3, lambda=NULL)
par(mfrow=c(2,3))
plot(object3)
plot(cv.obj3)
plot(coef(object3, m=cv.obj3$opt.step), cex=.5, ylab=expression(beta[i]))

# elasticNet same data with l1 parameter lambda=0.1
object4 <- l2boost(dta$x,dta$y,M=10000, nu=1.e-3, lambda=.1)

# 10 fold elasticNet CV
cv.obj4 <- cv.l2boost(dta$x,dta$y,M=10000, nu=1.e-3, lambda=.1)
plot(object4)
plot(cv.obj4)
plot(coef(object4, m=cv.obj4$opt.step), cex=.5, ylab=expression(beta[i]))
}
\references{
  Friedman (2001) Greedy function approximation: A gradient
  boosting machine. \emph{Annals of Statistics},
  29:1189-1232

  Efron B., Hastie T., Johnstone I., and Tibshirani R
  (2004). Least angle regression (with discussion).
  \emph{Annals of Statistics}, 32:407-499.

  Ehrlinger and Ishwaran (2012). Characterizing l2boosting.
  \emph{Annals of Statistics}, 40 (2), 1074-1101
}
\seealso{
  \code{\link{print.l2boost}}, \code{\link{plot.l2boost}},
  \code{\link{predict.l2boost}} methods of l2boost and
  \code{\link{cv.l2boost}}
}

