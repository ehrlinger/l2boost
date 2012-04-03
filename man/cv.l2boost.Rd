\name{cv.l2boost}
\alias{cv.l2boost}
\title{cv.l2boost K fold Cross Validation of the l2boost algorithm}
\usage{
  cv.l2boost(x, y, K = 10, M = NULL, nu = 1e-04,
    lambda = NULL, trace = FALSE,
    type = c("friedman", "discrete", "hybrid", "lars"),
    ...)
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

  \item{...}{Additional arguments to l2boost}
}
\description{
  cv.l2boost K fold Cross Validation of the l2boost
  algorithm
}
\examples{
sim.data <- elasticNetSim(n = 100) # build default elasticNet simulation
   x <- sim.data$x
   y <- sim.data$y

   cat("\\tCross Validated l2boost...\\t")
   l2.cv  <- cv.l2boost(x, y, nu = 1.e-3, M = 1.e4, type = "discrete")

   cat("\\tCross Validated elasticBoost...\\t")

   # Double CV over a range of lambda values
   eboost.cv  <- cv.l2boost(x, y, nu = 1.e-3, M = 1.e4, type = "discrete",
       lambda = 0.01)

    par(mfrow=c(2,2))
    plot(l2.cv)
    plot(l2.cv)
    plot(eboost.cv)
    plot(eboost.cv)
}
\seealso{
  \code{\link{l2boost}}, \code{\link{plot.l2boost}}
}

