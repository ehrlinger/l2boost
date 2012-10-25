#' Extract model coefficients from an l2boost model object.
#'  
#' @description \code{\link{coef}} is a generic function which extracts model coefficients from objects returned 
#' by modeling functions. 
#' 
#' By default, \code{\link{coef.l2boost}} returns the model (beta) coefficients from the last step, 
#' M of the \code{\link{l2boost}} model. For a \code{\link{cv.l2boost}} object, the default returns the coefficients from 
#' full model, at the cross-validation optimal step (\emph{m = opt.step} return value).
#'  
#' Coefficients from alternative steps can be obtained using the \emph{m} parameter. 
#'  
#' @param object an l2boost fit object
#' @param m the iteration number within the l2boost path. If m=NULL, the coefficients are obtained from the last iteration M.
#' @param ... generic arguments passed to parent function
#'
#' @seealso \code{\link{coef}} and \code{\link{l2boost}}, \code{\link{cv.l2boost}} and 
#'  \code{\link{predict.l2boost}} methods of l2boost
#' 
#' @return vector of coefficient estimates for \code{\link{l2boost}} objects. 
#' The estimates correspond to the given iteration \emph{m}.
#'
#' @examples
#' #--------------------------------------------------------------------------
#' # Example: Diabetes data 
#' #  
#' # See Efron B., Hastie T., Johnstone I., and Tibshirani R. 
#' # Least angle regression. Ann. Statist., 32:407-499, 2004.
#' data(diabetes, package='l2boost')
#' 
#' object <- l2boost(diabetes$x,diabetes$y, M=1000, nu=.01)
#' coef(object)
#' 
#' # At the m=500 step
#' coef(object, m=500)
#'
#' @method coef l2boost
#' @S3method coef l2boost
#' 
coef.l2boost <- function(object, m=NULL, ...){
  if(inherits(object, "cv")){
    if(!is.null(m)) warning ("Cannot select step number for cv.l2boost objects.")
    return(object$coef) 
  }else{
    if(is.null(m)){
      return(object$betam)
    }else{
      if(m <0) stop("Iteration step m >= 0")
      if(m >= length(object$betam.path)){
        warning(paste("Iteration selected beyond limit of m=", length(object$betam.path) -1, ". Reseting m=",
                      length(object$betam.path) -1))
        m =  length(object$betam.path) -1
      }
      return(object$betam.path[[m+1]])
    }
  }
}
