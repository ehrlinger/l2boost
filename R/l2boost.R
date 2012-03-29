
#---------------------------------------------------------------------
# l2boost (FRIEDMAN/DISCRETE/HYBRID/LARS)
# 
# notes:
# nu is ignored in LARS/LIMIT
# rescales x
# y is centered --- > mean is returned as part of the object
# !!! remove x-columns with NA's !!! CAUTION
#---------------------------------------------------------------------
#' l2boost implements the boosting using an l2-loss function
#'
#' l2boost is a fast implementation of Friedman's boosting algorithm with 
#' coordinate direction base learners and an l2-loss function.
#'
#' @title l2boost: linear regression boosting with an l2 loss function.
#' @param x design matrix of dimension n x p
#' @param y response variable of length n
#' @param M number of steps to run boost algorithm M >= p
#' @param nu l1 shrinkage parameter 0 < nu <= 1
#' @param lambda l2 shrinkage parameter used for elastic net boosting 0 < lambda
#' @param type = c("friedman", "discrete", "hybrid", "lars") 
#' @param qr.tolerance = 1e-30
#' @param eps.tolerance = .Machine$double.eps
#' @param trace = FALSE
#'
#' @export l2boost
l2boost <-
function(x, ...)UseMethod("l2boost")
