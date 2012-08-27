#---------------------------------------------------------------------
# l2boost print method
# SHOULD WORK FOR ALL l2boost variants
#---------------------------------------------------------------------
#' print method for l2boost objects
#'
#' @param x an l2boost object 
#' @param ... other arguments (not used)
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
#' print(object)
#' }
#'
#' @method print l2boost
#' @S3method print l2boost
print.l2boost <- function(x,...){
  cat("Call:\n")
  print(x$call)
  
  if(is.null(x$lambda)){
    cat("\nL2boost type:\t", x$type)
  }else{
    cat("\nelasticBoost type:\t", x$type)
  }
  
  cat("\nParameters:\n")
  M <- length(x$l.crit)
  nu <- x$nu
  cat("M = ", M, "\tnu = ", nu)
  if(!is.null(x$lambda)){
    cat("\t lambda = ", x$lambda)
  }
  if(inherits(x, "cv")){
    cat("\n K = ", x$K, " fold cross validation\n")
    cat("Optimal step = ", x$opt.step, "\tNorm = ", x$opt.norm, "\t MSE = ", x$mse)
  }
  cat("\n\nCoefficients:\n")
  if(inherits(x, "cv")){
    coeff =  list(coefficients = x$coef)
  }else{
    coeff = list(coefficients = x$betam)
  }
  names(coeff$coefficients) <-if(is.null(x$names)){paste("V",1:length(coeff$coefficients), sep="")}else{x$names}
  
  print(coeff$coefficients[which(abs(coeff$coefficients) > 0)]);
  
}
