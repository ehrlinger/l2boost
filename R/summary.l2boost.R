
#' Unimplemented generic function
#' These are placeholders right now.
#' @param object an l2boost object
#' @param ... other arguments (unused)
#' 
#' @importFrom stats pt
summary.l2boost <- function(object, ...){
  stop("Unimplemented function")
  se <- sqrt(diag(object$vcov))
  tval <- coef.l2boost(object) / se
  TAB <- cbind(Estimate = coef.l2boost(object), 
               StdErr = se, t.value = tval, 
               p.value = 2*pt(-abs(tval), 
                              df=object$df))
  res <- list(call=object$call, coefficients=TAB)
  class(res) <- "summary.linmod"
  res
}
