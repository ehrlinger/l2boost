
#---------------------------------------------------------------------
# cv wrapper for l2boost
# K-fold cross-validation for mean-squared prediction error
# makes use of mclapply
#---------------------------------------------------------------------
#' cv.l2boost K fold Cross Validation of the l2boost algorithm
#'
#' @param K = 10 number of cross validation folds
#'
#' @export cv.l2boost
#' @importFrom parallel mclapply
cv.l2boost <-
function(x, y, K = 10, M = NULL, nu = 1e-4, lambda = NULL, trace = FALSE, 
                       type = c("friedman", "discrete", "hybrid", "lars"),
                       ...) {
  # define the folds
  # last fold is the full data and corresponds to the "primary object"
  all.folds <- split(sample(1:length(y)), rep(1:K, length = length(y)))
  all.folds[[K+1]] <- length(y) + 1
  # multicore cv-call
  eval.fold.obj <- mclapply(1:(K+1), function(k) {
    eval.fold(k, K = K, all.folds = all.folds, x = x, y = y, M = M, nu = nu, lambda = lambda,
              trace = trace, type = type)})
  # extract the mse
  mse.list <- lapply(1:K, function(k) {eval.fold.obj[[k]]$mse})
  M.step <- max(sapply(1:K, function(k){length(mse.list[[k]])}), na.rm = TRUE)
  # rewrite the mse list in a matrix format more conducive for parsing 
  cv.all <-  matrix(NA, nrow = M.step, ncol = K)
  for (k in 1:K) {
    cv.all[1:length(mse.list[[k]]), k] <- mse.list[[k]]
  }
  # determine the optimal number of steps
  mse.avg <- apply(cv.all, 1, mean, na.rm = TRUE)
  opt.step <- min(which(mse.avg == min(mse.avg, na.rm = TRUE)))
  mse <- mse.avg[opt.step]
  # prediction using full data for the optimal number of steps
  fit.all <- eval.fold.obj[[K+1]]$obj
  yhat <- predict.l2boost(fit.all)$yhat.path[[opt.step]]
  pred <- predict.l2boost(fit.all, type = "coef")
  opt.norm <- sum(abs(pred$coef.path[[opt.step]]), na.rm = TRUE)
  # return the object
  object <- list(obj = fit.all,
                 mse = mse,
                 mse.list = mse.list,
                 yhat = yhat,
                 coefficients = pred$coef.path[[opt.step]],
                 coefficients.stand = pred$coef.stand.path[[opt.step]],
                 opt.step = opt.step,
                 opt.norm = opt.norm)
  class(object) <- c("l2boost", "cv")
  invisible(object)
}
