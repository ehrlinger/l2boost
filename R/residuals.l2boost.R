#' Get model residuals for an l2boost object
#'
#' @param object an l2boost object for the extraction 
#' of model coefficients.
#' @param m the iteration number with the l2boost path. 
#' If m=NULL, the coefficients are obtained from the last iteration M.
#' @param ... arguments (unused)
#'
#' @return a vector of n residuals
#'
#' @S3method residuals l2boost
#' @export residuals.l2boost
residuals.l2boost <-
function(object, m=NULL, ...){

 prd <- predict(object)
 rnms <- if(is.null(rownames(object$x))){1:dim(object$x)[1]}else{rownames(object$x)}
 if(is.null(m)){
   rs = as.vector(object$Fm - prd$yhat)
 }else{
   if(m <0) stop("Iteration step m >= 0")
   if(m > length(object$Fm))stop("Iteration selected beyond limit of m=", length(object$Fm.path) -1)
   rs<- as.vector(object$Fm.path[[m+1]] - prd$yhat.path[[m+1]])
 }
   names(rs) <- rnms
   return(rs)
}
