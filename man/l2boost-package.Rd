\docType{package}
\name{l2boost-package}
\alias{l2boost-package}
\title{Efficient implementation of Freidman's boosting algorithm for linear regression using an
 l2-loss function and coordinate direction (design matrix columns) basis functions.}
\description{
  The l2boost package implements a generic boosting method
  [Friedman (2001)] for linear regression settings using an
  l2-loss function. The basis functions are simply the
  column vectors of the design matrix.
  \code{\link{l2boost}} scales the design matrix such that
  the boosting coefficients correspond to the gradient
  direction for each covariate. Friedman's gradient descent
  boosting algorithm proceeds at each step along the
  covariate direction closest (in L2 distance) to the
  maximal gradient descent direction.

  The \code{\link{l2boost}} function uses an arbitrary
  L1-regularization parameter (nu), and includes the
  elementary data augmentation of Ehrlinger and Ishwaran
  (2012), to add an L2-penalization (lambda) similar to the
  elastic net [Zou and Hastie (2005)]. The
  L2-regularization reverses repressibility, a condition
  where one variable acts as a boosting surrogate for
  other, possibly informative, variables. Along with the
  decorrelation effect, this elasticBoost regularization
  circumvents L2Boost deficiencies in correlated settings.

  We include a series of S3 functions for working with
  \code{\link{l2boost}} objects: \itemize{ \item
  \code{\link{print}} (\code{\link{print.l2boost}}) prints
  a summary of the fit, \item \code{\link{coef}}
  (\code{\link{coef.l2boost}}) returns the model regression
  coefficients. \item \code{\link{fitted}}
  (\code{\link{fitted.l2boost}}) returns the fitted
  response values from the training set, \item
  \code{\link{residuals}} (\code{\link{residuals.l2boost}})
  returns the training set residuals, \item
  \code{\link{plot}} (\code{\link{plot.l2boost}}) for
  graphing, \item \code{\link{predict}}
  (\code{\link{predict.l2boost}}) for prediction on
  possibly new observations, } A cross-validation method
  (\code{\link{cv.l2boost}}) is also included for L2boost
  and elasticBoost cross-validating regularization
  parameter optimizations.

  \emph{Example Datasets} We have repackaged the
  \code{\link{diabetes}} data set from Efron et. al. (2004)
  for demonstration purposes. We also include simulation
  functions for reproducing the elastic net simulation
  (\code{\link{elasticNetSim}}) of Zou and Hastie (2005)
  and the example multivariate normal simulations
  (\code{\link{mvnorm.l2boost}}) of Ehrlinger and Ishwaran
  (2012).
}
\references{
  Friedman J. (2001) Greedy function approximation: A
  gradient boosting machine. \emph{Ann. Statist.},
  29:1189-1232

  Ehrlinger J., and Ishwaran H. (2012). "Characterizing
  l2boosting" \emph{Ann. Statist.}, 40 (2), 1074-1101

  Zou H. and Hastie T (2005) "Regularization and variable
  selection via the elastic net" \emph{J. R. Statist. Soc.
  B}, 67, Part 2, pp. 301-320

  Efron B., Hastie T., Johnstone I., and Tibshirani R.
  (2004). "Least Angle Regression" \emph{Ann. Statist.}
  32:407-499
}

