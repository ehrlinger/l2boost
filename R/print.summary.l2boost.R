
#' Unimplemented generic function
#' These are placeholders right now.
print.summary.l2boost <-
function(x, ...){
 stop("Unimplemented function")
 cat("Call:\n")
 print(x$call)
 cat("\n")
 printCoefmat(x$coefficients, P.value=TRUE, has.Pvalue=TRUE)
}
