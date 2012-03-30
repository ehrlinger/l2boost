#' l2boost taking a formula object instead of design matrix.
#'
#' @param formula an object of class \code{\link{formula}} 
#'     (or one that can be coerced to that class): a symbolic 
#'     description of the model to be fitted. The details of 
#'     model specification are given under "Details".
#' @param data an optional data frame, list or environment 
#'    (or object coercible by \code{\link{as.data.frame}} to 
#'    a data frame) containing the variables in the model. 
#'    If not found in data, the variables are taken from 
#'    "environment(formula)", typically the environment from 
#'    which \code{\link{lm}} is called.
#'
# @seealso \code{\link{l2boost}}
#' 
#' @rdname l2boost
#' @S3method l2boost formula
l2boost.formula <-
function(formula, data=list(), ...){  
  mf <- model.frame(formula=formula, data=data)
  x<- model.matrix(attr(mf, "terms"), data=mf)
  y<-model.response(mf)

  est<- l2boost.default(x,y, ...)
  est$call<-match.call()
  est$formula <-formula
  invisible(est)
}
