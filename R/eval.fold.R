#' core cv function
eval.fold <-
function(k, K, all.folds, x, y, M, nu, lambda, trace, type) {  
  if (trace) {
    if (k <= K) {
      cat("\t K-fold:", k, "\n")
    }
    else {
      cat("\t final analysis (full-data)\n")
    }
  }
  omit <- all.folds[[k]]
  fit <- l2boost(x = as.matrix(x[-omit,, drop = FALSE]), y = y[-omit],
                 M = M, nu = nu, type = type, lambda = lambda)
  if (k <= K) {
    yhat.path <- predict.l2boost(fit, xnew = as.matrix(x[omit, ]))$yhat.path
    mse <- sapply(1:length(yhat.path), function(m) {
        mean((yhat.path[[m]] - y[omit])^2, na.rm = TRUE)})
    return(list(obj = fit, mse = mse)) 
  }
  else {
    return(list(obj = fit))    
  }
}
