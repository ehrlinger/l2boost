#' predict method for l2boost objects.
#'
#' @param obj an l2boost object
#' @param xnew a new design matrix to fit with the l2boost obj
#' @param newdata a new design matrix to fit with the l2boost obj
#' @param type fit returns the predicted values coef returns the model coefficients
#'
#' @S3method predict l2boost
predict.l2boost <-
function(obj, xnew = NULL, type = c("fit", "coef"), newdata=xnew) {
 
  type <- match.arg(type)
  # extract necessary items from the stagewise object
  ybar <- obj$ybar
  betam <- betam.stand <- obj$betam
  betam.path <- betam.stand.path <- obj$betam.path
  x.na <- obj$x.na
  M <- length(betam.path)
  p <- length(betam)
  if (any(obj$x.na) & !is.null(xnew)) {
    xnew <- xnew[, !x.na]
  }
  if (is.null(xnew)) xnew <- obj$x
  x.attr <- obj$x.attr
  n <- x.attr$dim[1]
  n.new <- nrow(xnew)
  if (type == "fit") {
    # center and rescale xnew using original data
    # !!! do NOT remove NA columns !!!
    xnew <- scale(xnew, center = x.attr$"scaled:center", scale = x.attr$"scaled:scale")/sqrt(n - 1)
    yhat.path <- lapply(1:M, function(m) {
          be.m <- betam.stand.path[[m]]
          pt.m <- which(abs(be.m) > .Machine$double.eps)
          if (sum(pt.m) > 0) {
             yhat.m <- c(ybar + as.matrix(xnew[, pt.m]) %*% be.m[pt.m])
          }
          else {
            yhat.m <- rep(ybar, n.new)
          }
          yhat.m
        })
    yhat <- yhat.path[[M]]
    #return the predictor
    return(list(yhat = yhat, yhat.path = yhat.path))
  }
  else if (type == "coef") {
    # rescale coefficients back to original x-variable scale
    sf <- x.attr$"scaled:scale" *  sqrt(n - 1)
    betam <- betam.stand / sf
    for (k in 1:M) {
      betam.path[[k]] <- betam.stand.path[[k]] / sf
    }
    return(list(coef = betam,
                coef.stand = betam.stand,
                coef.path = betam.path,
                coef.stand.path = betam.stand.path))
  }
  else {
    stop("type must be set to 'fit' or 'coef'\n")
  }
}
