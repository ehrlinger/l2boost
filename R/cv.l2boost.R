
#---------------------------------------------------------------------
# cv wrapper for l2boost
# K-fold cross-validation for mean-squared prediction error
# makes use of mclapply
#---------------------------------------------------------------------
#' cv.l2boost K fold Cross Validation for l2boost
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
#' @param cores number of cores to parallel the cv analysis.
#' @param ... Additional arguments to l2boost
#'
#' @seealso \code{\link{l2boost}}, \code{\link{plot.l2boost}}, \code{\link{predict.l2boost}}
#'
#' @return A list of cross validation results:
#'  \item{call}{the matched call.}   
#'  \item{type}{Choice of l2boost algorithm from "friedman", "discrete", "hybrid", "lars"}        
#'  \item{names}{design matrix column names}         
#'  \item{nu}{The l1 boosting shrinkage parameter value}          
#'  \item{lambda}{The l2 elasticNet shrinkage parameter value}                   
#'  \item{K}{number of folds for cross-validation}             
#'  \item{mse}{Optimal cross-validation mean square error}      
#'  \item{mse.list}{list of K vectors of m mean square errors}            
#'  \item{coef}{beta estimates from the full model at opt.step}     
#'  \item{coef.stand}{standardized beta estimates from full model at opt.step}    
#'  \item{opt.step}{optimal step m calculated by cross-validation}           
#'  \item{opt.norm}{}        
#'  \item{obj}{l2boost fit of full model}         
#'  \item{yhat}{estimate of response from full model at opt.step}      
#' 
#' @examples
#' \dontrun{
#' #--------------------------------------------------------------------------
#' # Example 1: ElasticBoost simulation
#' # Compare l2boost and elasticNetBoosting using 10-fold CV
#' # 
#' # See Zou H. and Hastie T. Regularization and variable selection via the 
#' # elastic net. J. Royal Statist. Soc. B, 67(2):301-320, 2005
#' dta <- elasticNetSim(n=100)
#' Mtarget=10000
#' # l2boost the simulated data with groups of correlated coordinates
#' object3 <- l2boost(dta$x,dta$y,M=Mtarget, nu=1.e-3, lambda=NULL)
#' # 10 fold l2boost CV  
#' cv.obj3 <- cv.l2boost(dta$x,dta$y,M=Mtarget, nu=1.e-3, lambda=NULL)
#' par(mfrow=c(2,3))
#' plot(object3)
#' plot(cv.obj3)
#' plot(coef(object3, m=cv.obj3$opt.step), cex=.5, ylab=expression(beta[i]))
#'
#' # elasticNet same data with l1 parameter lambda=0.1
#' object4 <- l2boost(dta$x,dta$y,M=Mtarget, nu=1.e-3, lambda=.1) 
#' # 10 fold elasticNet CV
#' cv.obj4 <- cv.l2boost(dta$x,dta$y,M=Mtarget, nu=1.e-3, lambda=.1) 
#' plot(object4)
#' plot(cv.obj4)
#' plot(coef(object4, m=min(cv.obj4$opt.step, Mtarget), cex=.5, ylab=expression(beta[i]))
#' }
#'
#' @export cv.l2boost
#' @importFrom parallel mclapply
cv.l2boost <- function(x, y, K = 10, M = NULL, nu = 1e-4, lambda = NULL, trace = FALSE, 
                       type = c("friedman", "discrete", "hybrid", "lars"), cores=NULL,
                       ...) {
  call<-match.call()
  n<- length(y)
  
  # test for multicores
  if(is.null(cores)){
    num <- detectCores()
    # If the user doesn't know, then leave 1 core for OS type stuff. So the system
    # remains responsive.
    if(num > 1){
      cores = num-1
    }else{
      cores = 1
    }
  }else{
    if(cores > detectCores()){
      # it is ok to request more cores than the machine has, and can improve performance in
      # some instances. However, I'll still drop a warning.
      warning(paste("This CV analysis attempts to use more cores (", cores, ") than available (", 
                    detectCores(), "). \n !! This is not an error !!, analysis continuing."))
    } 
  }
  
  # Test for error conditions
  if(K > n) stop(paste("Number of folds (K=", K
                       , ") less than the number of observations in the design matrix (n=",
                       n, ")"))
  # define the folds
  # last fold is the full data and corresponds to the "primary object"
  all.folds <- split(sample(1:n), rep(1:K, length = n))
  all.folds[[K+1]] <- length(y) + 1
  
  if(cores ==1){
    # Single core function.
    eval.fold.obj <-lapply(1:length(all.folds), function(k) {
      eval.fold(k, K = K, all.folds = all.folds, x = x, y = y, M = M, nu = nu,
                lambda = lambda, trace = trace, type = type, ...=...)})
  }else{
    # clusterApply() for Windows
    if (Sys.info()[1] == "Windows"){
      cl <- makeCluster(cores)
      eval.fold.obj <-clusterApply(cl=cl, k=1:length(all.folds), function(k) {
        eval.fold(k, K = K, all.folds = all.folds, x = x, y = y, M = M, nu = nu,
                  lambda = lambda, trace = trace, type = type, ...=...)})
      stopCluster(cl) # Don't forget to do this--I frequently do
      
      # mclapply() for everybody else
    } else {
      # multicore cv-call
      eval.fold.obj <- mclapply(1:length(all.folds), function(k) {
        eval.fold(k, K = K, all.folds = all.folds, x = x, y = y, M = M, nu = nu,
                  lambda = lambda, trace = trace, type = type, ...=...)}, mc.cores=cores)
    }
  }
  #print(eval.fold.obj)
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
  object <- list(call=call, type=type,nu=nu,K=K,lambda=lambda, 
                 obj = fit.all,
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
