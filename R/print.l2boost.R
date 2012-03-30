
#---------------------------------------------------------------------
# l2boost print method
# SHOULD WORK FOR ALL l2boost variants
#---------------------------------------------------------------------
#' print.l2boost
#'
#' @param x an l2boost object 
#' @param ... other arguments (not used)
#'
#' @S3method print l2boost

print.l2boost <-
function(x,...){
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

  cat("\n\nCoefficients:\n")
  coeff = list(coefficients = x$betam)
  names(coeff$coefficients) <-if(is.null(x$names)){paste("V",1:dim(x$x)[2], sep="")}else{x$names}
  print(coeff$coefficients[which(abs(coeff$coefficients) > 0)]);

}
