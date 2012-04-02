#' Plots l2boost objects
#'
#' @param obj l2boost or cv.l2boost object
#' @param type which type of plot. \emph{rho} shows gradient correlation, \emph{coef} regression (beta) coefficients
#' @param standardize Should we plot standardized gradient correlation (default: TRUE)
#' @param active.set Vector of indices of the coordinates for highlighting with color=col (default: NULL shows all active coordinates)
#' @param xvar what measure do we plot on the x-axis? \emph{step} plots the step m, \emph{norm} plots the normalized distance (1-nu)^(m-1)
#' @param x.lab specific x-axis label (NULL results in default value depending on xvar)
#' @param y.lab specific y-axis label (NULL results in default value depending on type)
#' @param trim (default: TRUE)
#' @param clip Do we want to c
#' @param col Color to highlight active.set coordinates (NULL indicates default all active set at step M in blue, changes to red after selection
#' @param ylim Control plotted y-values (default: NULL for auto range)
#' @param xlim Control plotted x-values (default: NULL for auto domain )
#' @param ... other arguments
#'
#' @return \code{NULL}
#'
#' @S3method plot l2boost

plot.l2boost <-
function(obj,
                         type = c("rho", "coef"),
                         standardize = TRUE, active.set=NULL,
                         xvar = c("step", "norm"),
                         x.lab = NULL, y.lab = NULL,
                         trim = TRUE, clip=NULL, col=NULL,ylim=NULL, xlim=NULL, ...) {
  # preliminary checks
  if (class(obj)[1] != "l2boost") stop("This function only works for objects of class `l2boost'")
  type <- match.arg(type)
  xvar <- match.arg(xvar)
  # ----------------------------------------------------------------------------
  # cv plots
  # ----------------------------------------------------------------------------
  if (class(obj)[2] == "cv") {
    # convert mse into matrix format more conducive for plotting/printing
    mse <- obj$mse.list
    K <- length(mse)
    M <- max(sapply(1:K, function(k){length(mse[[k]])}), na.rm = TRUE)
    cv.all <-  matrix(NA, nrow = M, ncol = K)
    x.range <- rep(NA, K)
    for (k in 1:K) {
      cv.all[1:length(mse[[k]]), k] <- mse[[k]]
      x.range[k] <- length(mse[[k]])
    }
    step.size <- c(1, max(x.range, na.rm = TRUE))
    cv <- apply(cv.all, 1, mean, na.rm = TRUE)
    cv.error <- sqrt(apply(cv.all, 1, VAR)/K)
    #trim the y-axis
    if (trim) {
      if (K <= 5) {
        y.range <- quantile(c(cv.all, cv, cv + cv.error, cv - cv.error), c(0, .95), na.rm = TRUE)
      }
      else {
        y.range <- quantile(c(cv, cv + cv.error, cv - cv.error), c(0, .95), na.rm = TRUE)
      }
    }
    else {
      if (K <= 5) {
        y.range <- range(c(cv.all, cv, cv + cv.error, cv - cv.error), na.rm = TRUE)
      }
      else {
        y.range <- range(c(cv, cv + cv.error, cv - cv.error), na.rm = TRUE)
      }
    }
    if (is.null(x.lab)) x.lab <- "steps"
    if (is.null(y.lab)) y.lab <- "cross-validated MSE"
    matplot(1:M, cv.all, type = c("l", "n")[1 + 1 * (K > 5)], lty = 3, col = 2, lwd = 0.05,
         xlim = range(step.size),
         ylim = y.range,
         xlab = x.lab, ylab = y.lab)
    lines(1:M, cv, lty = 1, lwd = 5, col = 2)
    error.bars(1:M, cv + cv.error, cv - cv.error, width = 0.0025, col = "gray")
    cat("minimum cross-validated MSE equals", round(obj$mse, 4), "for step size", obj$opt.step, "\n")
  }
  else {
    y <- obj$y
    Fm.path <- obj$Fm.path
    rhom.path <- obj$rhom.path
    M <- length(rhom.path)
    p <- length(obj$betam)
    l.crit <- obj$l.crit

    # determine what goes on the x-axis: (i) step (ii) norm
    if (xvar == "step") {
      xval <- 1:M
      if (is.null(x.lab)) x.lab <- "step"
    }
    else {
      b.m.path <- predict.l2boost(obj, type = "coef")$coef.path
      xval <- sapply(1:M, function(m) {sum(abs(b.m.path[[m]]), na.rm = TRUE)})
      if (is.null(x.lab)) x.lab <- "l1-norm"
    }
      
    # ----------------------------------------------------------------------------
    # rho path plots
    # ----------------------------------------------------------------------------
    if (type == "rho") {
      if (is.null(y.lab)) y.lab <- "gradient"
      if (standardize) {
        rhom.path <- lapply(1:M, function(m){rhom.path[[m]]/sqrt(sum((y - Fm.path[[m]])^2, na.rm = TRUE))})
        if (is.null(y.lab)) y.lab <- "gradient-correlation"
      }
      path <- rhom.path
    }
    # ----------------------------------------------------------------------------
    # coef path plots
    # ----------------------------------------------------------------------------
    else if (type == "coef") {
      if (standardize) {
        if (is.null(y.lab)) y.lab <- "standardized coefficients"   
        path <- predict.l2boost(obj, type = "coef")$coef.stand.path
      }
      else {
        if (is.null(y.lab)) y.lab <- "coefficients"   
        path <- predict.l2boost(obj, type = "coef")$coef.path
      }
    }
    else {
      stop("type must be set to 'rho' or 'coef'\n")
    }
    # plot it
    if(is.null(xlim))xlim <- range(xval)
    if(is.null(ylim))ylim <- range(unlist(path))
    if(is.null(active.set)) active.set <- unique(l.crit)
    plot(range(xval), range(unlist(path)), type = "n",
         xlab = x.lab,
         xlim = xlim,
         ylab = y.lab,
         ylim = ylim)
    #plot inactive set first
    if (type == "rho") {
      plot.lines(xval, setdiff(1:p, active.set), path, l.crit, FALSE, col)
      plot.lines(xval, active.set, path, l.crit, TRUE, col)
    }
    else {
      plot.lines(xval, active.set, path, l.crit, FALSE)
    }
  }
}
