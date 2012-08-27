\name{cv.l2boost}
\alias{cv.l2boost}
\title{cv.l2boost K fold Cross Validation for l2boost}
\usage{
  cv.l2boost(x, y, K = 10, M = NULL, nu = 1e-04,
    lambda = NULL, trace = FALSE,
    type = c("friedman", "discrete", "hybrid", "lars"),
    cores = NULL, ...)
}
\arguments{
  \item{K}{number of cross validation folds (default: 10)}

  \item{x}{the design matrix}

  \item{y}{the response vector}

  \item{M}{the total number of iterations to boost.  If
  NULL, M is set to minimum of n or p from design matrix}

  \item{nu}{l1 shrinkage paramater (default: 1e-4)}

  \item{lambda}{l2 shrinkageparameter (default: NULL)}

  \item{trace}{Show computation output? (default: FALSE)}

  \item{type}{Type of l2boost fit with (default: freidman)}

  \item{cores}{number of cores to parallel the cv
  analysis.}

  \item{...}{Additional arguments passed to
  \code{\link{l2boost}}}
}
\value{
  A list of cross validation results: \item{call}{the
  matched call.} \item{type}{Choice of l2boost algorithm
  from "friedman", "discrete", "hybrid", "lars"}. see
  \code{\link{l2boost}} \item{names}{design matrix column
  names} \item{nu}{The l1 boosting shrinkage parameter
  value} \item{lambda}{The l2 elasticNet shrinkage
  parameter value} \item{K}{number of folds for
  cross-validation} \item{mse}{Optimal cross-validation
  mean square error} \item{mse.list}{list of K vectors of m
  mean square errors} \item{coef}{beta estimates from the
  full model at opt.step} \item{coef.stand}{standardized
  beta estimates from full model at opt.step}
  \item{opt.step}{optimal step m calculated by
  cross-validation} \item{opt.norm}{} \item{obj}{l2boost
  fit of full model} \item{yhat}{estimate of response from
  full model at opt.step}
}
\description{
  cv.l2boost K fold Cross Validation for l2boost
}
\examples{
\dontrun{
#--------------------------------------------------------------------------
# Example 1: ElasticBoost simulation
# Compare l2boost and elasticNetBoosting using 10-fold CV
#
# See Zou H. and Hastie T. Regularization and variable selection via the
# elastic net. J. Royal Statist. Soc. B, 67(2):301-320, 2005
dta <- elasticNetSim(n=100)
Mtarget=10000
# l2boost the simulated data with groups of correlated coordinates
object3 <- l2boost(dta$x,dta$y,M=Mtarget, nu=1.e-3, lambda=NULL)
# 10 fold l2boost CV
cv.obj3 <- cv.l2boost(dta$x,dta$y,M=Mtarget, nu=1.e-3, lambda=NULL)
par(mfrow=c(2,3))
plot(object3)
plot(cv.obj3)
plot(coef(object3, m=cv.obj3$opt.step), cex=.5, ylab=expression(beta[i]))

# elasticNet same data with l1 parameter lambda=0.1
object4 <- l2boost(dta$x,dta$y,M=Mtarget, nu=1.e-3, lambda=.1)
# 10 fold elasticNet CV
cv.obj4 <- cv.l2boost(dta$x,dta$y,M=Mtarget, nu=1.e-3, lambda=.1)
plot(object4)
plot(cv.obj4)
plot(coef(object4, m=min(cv.obj4$opt.step, Mtarget), cex=.5, ylab=expression(beta[i]))
}
}
\seealso{
  \code{\link{l2boost}}, \code{\link{plot.l2boost}},
  \code{\link{predict.l2boost}}
}

