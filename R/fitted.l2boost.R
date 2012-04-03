
#' fitted.l2boost returns the function F_m(x) evaluated at 
#' iteration m using the training data set x
#' 
#' @param object an l2boost object for the extraction of 
#' model coefficients.
#' @param m the iteration number with the l2boost path. 
#' If m=NULL, the coefficients are obtained from the last iteration M.
#' @param ... other arguments
#'
#'
#' @S3method fitted l2boost
#' @export fitted.l2boost
fitted.l2boost <-
function(object, m=NULL, ...){
 rnms <- if(is.null(rownames(object$x))){1:dim(object$x)[1]}else{rownames(object$x)}

 if(is.null(m)){
   rs<-as.vector(object$Fm)
 }else{
   if(m <0) stop("Iteration step m >= 0")
   if(m > length(object$Fm))stop("Iteration selected beyond limit of m=", length(object$Fm.path) -1)
   rs<-as.vector(object$Fm.path[[m+1]])
 }
 names(rs) <- rnms
 return(rs)

}
