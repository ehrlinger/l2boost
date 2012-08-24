#' coef method for l2boost objects extracts model coefficients from an l2boost fit object.
#' 
#' @param object an l2boost fit object
#' @param m the iteration number within the l2boost path. If m=NULL, the coefficients are obtained from the last iteration M.
#' @param ... generic arguments passed to parent function
#'
#' @seealso \code{\link{coef}} and \code{\link{l2boost}},\code{\link{cv.l2boost}}, \code{\link{predict.l2boost}} methods of l2boost
#' 
#' @return the un-standardized coefficient estimates for l2boost objects. Ov objrct is a cv.l2boost, 
#' return the unstandardized coefficients at the opt.step of the full model.
#'
#' @examples
#' \dontrun{
#' #--------------------------------------------------------------------------
#' # Example 1: Diabetes 
#' #  
#' # See Efron B., Hastie T., Johnstone I., and Tibshirani R. 
#' # Least angle regression. Ann. Statist., 32:407-499, 2004.
#' data(diabetes)
#' 
#' object <- l2boost(diabetes$x,diabetes$y, M=1000, nu=.01)
#' coef(object)
#' }
#'
#' @method coef l2boost
#' 
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
