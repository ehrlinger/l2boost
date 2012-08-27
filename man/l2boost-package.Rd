\docType{package}
\name{l2boost-package}
\alias{l2boost-package}
\title{l2boost - Boosting linear regression models}
\description{
  An boosting implementation for linear regression models
  using coordinate directions (design matrix columns) as
  basis functions and an l2-loss function. The
  \code{\link{l2boost}} function includes the data
  augmentation of Ehrlinger and Ishwaran (2012) to
  implement the elasticBoost extensions.
}
\details{
  We include a series of S3 functions for working with
  l2boost objects (\code{\link{plot.l2boost}},
  \code{\link{predict.l2boost}},
  \code{\link{print.l2boost}},
  \code{\link{fitted.l2boost}},
  \code{\link{residuals.l2boost}},
  \code{\link{coef.l2boost}}). A cross validation method
  (\code{\link{cv.l2boost}}) is also included for l2boost
  and elasticBoost cross validation.

  We have repackaged the \code{\link{diabetes}} data set
  from Efron et. al. (2004), and included simulation
  functions for reproducing the elasticNet simulation
  (\code{\link{elasticNetSim}}) of Zou and Hastie (2005)
  and the example simulations
  (\code{\link{mvnorm.l2boost}}) from Ehrlinger and
  Ishwaran (2012).
}
\references{
  John Ehrlinger, Hemant Ishwaran (2012). Characterizing
  l2boosting. \emph{Annals of Statistics}, 40 (2),
  1074-1101

  Efron B., Hastie T., Johnstone I., and Tibshirani R.
  "Least Angle Regression" \emph{Annals of Statistics}
  32:407-499, 2004.

  Hui Zou, Trevor Hastie (2005) Regularization and variable
  selection via the elastic net \emph{J. R. Statist. Soc.
  B}, 67, Part 2, pp. 301-320
}

