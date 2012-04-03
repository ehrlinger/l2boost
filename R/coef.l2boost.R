#' coef.l2boost method for l2boost objects which extracts model coefficients.
#' 
#' @param object an l2boost object
#' @param m the iteration number with the l2boost path. If m=NULL, the coefficients are obtained from the last iteration M.
#' @param ... generic arguments passed to parent function
#'
#' @S3method coef l2boost
#' @export coef.l2boost
coef.l2boost <-
function(object, m=NULL, ...){
 if(is.null(m)){
   return(object$betam)
 }else{
   if(m <0) stop("Iteration step m >= 0")
   if(m > length(object$betam.path))stop("Iteration selected beyond limit of m=", length(object$betam.path) -1)
   return(object$betam.path[[m+1]])
 }
}
