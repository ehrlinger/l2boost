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
#' @seealso \code{\link{residuals}} and \code{link{l2boost}} and \code{\link{predict.l2boost}}
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
#' rsd<-residuals(object)
#'
#' # This is equivalent to the difference between the response and training set 
#' # estimates
#' #cbind(rsd, y-predict(object), rsd == y-predict(object))
#' }
#'
#' @method residuals l2boost
#' @S3method residuals l2boost
residuals.l2boost <-
  function(object, m=NULL, ...){
    if(inherits(object, "cv")) object<- object$obj
    
    prd <- predict(object)
    rnms <- if(is.null(rownames(object$x))){1:dim(object$x)[1]}else{rownames(object$x)}
    if(is.null(m)){
      rs = as.vector(object$Fm - prd$yhat)
    }else{
      if(m <0) stop("Iteration step m >= 0")
      if(m > length(object$Fm)){
        warning(paste("Iteration selected beyond limit of m=", length(object$Fm.path) -1, 
                      ". Reseting m=", length(object$Fm.path) -1))
                m =  length(object$Fm.path) -1
      }
      rs<- as.vector(object$Fm.path[[m+1]] - prd$yhat.path[[m+1]])
    }
    names(rs) <- rnms
    return(rs)
  }
