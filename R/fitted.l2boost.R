#' returns the fitted function values of an l2boost model object, F_m(x) evaluated at 
#' iteration m using the training data set x
#' 
#' @param object an l2boost object for the extraction of 
#' model coefficients.
#' @param m the iteration number with the l2boost path. 
#' If m=NULL, the coefficients are obtained from the last iteration M.
#' @param ... other arguments
#'
#' @seealso \code{\link{fitted}} and \code{link{l2boost}}
#'
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
#' fitted(object)
#' }
#' 
#' @method fitted l2boost
#' @S3method fitted l2boost
fitted.l2boost <- function(object, m=NULL, ...){
  if(inherits(object, "cv")){
    if(is.null(m)){
      rs<-predict(object$obj)$yhat.path[[object$opt.step]]
    }else{
      rs<-predict(object$obj)$yhat.path[[m]]
    }
  }else{
    rnms <- if(is.null(rownames(object$x))){1:dim(object$x)[1]}else{rownames(object$x)}
    
    if(is.null(m)){
      rs<-as.vector(object$Fm)
    }else{
      if(m <0) stop("Iteration step m >= 0")
      if(m > length(object$Fm)){
        warning(paste("Iteration selected beyond limit of m=", length(object$Fm.path) -1,
                      ". Reseting m=", length(object$Fm.path) -1))
        m =  length(object$Fm.path) -1
      }
      rs<-as.vector(object$Fm.path[[m+1]])
    }
    
    names(rs) <- rnms
  }
  return(rs)
}
