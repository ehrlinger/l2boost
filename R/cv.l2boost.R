
#---------------------------------------------------------------------
# cv wrapper for l2boost
# K-fold cross-validation for mean-squared prediction error
# makes use of mclapply
#---------------------------------------------------------------------
#' cv.l2boost K fold Cross Validation of the l2boost algorithm
#'
#' @param K number of cross validation folds (default: 10)
#' @param x the design matrix
#' @param y the response vector
#' @param M the total number of iterations to boost. 
#'    If NULL, M is set to minimum of n or p from design matrix
#' @param nu l1 shrinkage paramater (default: 1e-4)
#' @param lambda l2 shrinkageparameter (default: NULL)
#' @param trace Show computation output? (default: FALSE)
#' @param type Type of l2boost fit with (default: freidman)
#' @param ... Additional arguments to l2boost
#'
#' @seealso \code{\link{l2boost}}, \code{\link{plot.l2boost}}
#'
#' @examples
#'    sim.data <- elasticNet.sim(n = 100) # build default elasticNet simulation
#'    x <- sim.data$x
#'    y <- sim.data$y
#'  
#'    cat("\tCross Validated l2boost...\t")
#'    l2.cv  <- cv.l2boost(x, y, nu = 1.e-3, M = 1.e4, type = "discrete")
#'    l2.coef<- l2.cv$coefficients
#'    l2.opt.step <- l2.cv$opt.step
#'    l2.mse <- l2.cv$mse
#'
#'  
#'    cat("\tCross Validated elasticBoost...\t")
#' 
#'    # Double CV over a range of lambda values 
#'    lambda.seq <- c(0.00001, 0.001, 0.01, seq(0.05, 1, by = 0.05)) 
#'    n.lambda <- length(lambda.seq)
#'    eboost.cv.list <- vector("list", length = n.lambda)
#'    mse <- rep(NA, n.lambda)
#'    for (k in 1:n.lambda) {
#'      eboost.cv.list[[k]]  <- cv.l2boost(x, y, nu = 1.e-3, M = 1.e4, type = "discrete", 
#'        lambda = lambda.seq[k])
#'      mse[k] <- eboost.cv.list[[k]]$mse
#'      }
#'     which.opt.lambda <- min(which(mse == min(mse, na.rm = TRUE)))
#' 
#'     # Now refit the optimal step with elasticBoost 
#'     eboost.obj <- l2boost(x, y,  nu = 1.e-3, M = 1.e4,  trace = FALSE,
#'         lambda = lambda.seq[which.opt.lambda])
#'
#'     par(mfrow=c(2,2))
#'     plot(l2.cv)
#'     plot(eboost.cv.list[[which.opt.lambda]])
#' 
#'     plot(eboost.obj)
#
#     boxplot(t(eboost.coef), xlab = "variable", ylab = "coefficient estimate", outline = TRUE)
#     points(apply(eboost.coef, 1, mean), col = 2, pch = 15, cex = 0.5)
#     boxplot(t(l2.coef), xlab = "variable", ylab = "coefficient estimate", outline = TRUE)
#     points(apply(l2.coef, 1, mean), col = 2, pch = 15, cex = 0.5)
#
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
  # add names to the optimal coefficient path
  opt.coef.path <- pred$coef.path[[opt.step]]
  opt.coef.stand.path <- pred$coef.stand.path[[opt.step]]
  names(opt.coef.path) <- names(opt.coef.stand.path) <- fit.all$names
 
  # return the object
  object <- list(obj = fit.all,
                 mse = mse,
                 mse.list = mse.list,
                 yhat = yhat,
                 coef = opt.coef.path,
                 coef.stand = opt.coef.stand.path,
                 opt.step = opt.step,
                 opt.norm = opt.norm,
                 names = fit.all$names)
  class(object) <- c("l2boost", "cv")
  invisible(object)
}
