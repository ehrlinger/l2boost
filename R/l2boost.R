
#---------------------------------------------------------------------
# l2boost (FRIEDMAN/DISCRETE/HYBRID/LARS)
# 
# notes:
# nu is ignored in LARS/LIMIT
# rescales x
# y is centered --- > mean is returned as part of the object
# !!! remove x-columns with NA's !!! CAUTION
#---------------------------------------------------------------------
#' @export l2boost
#' @alias l2boost
l2boost <-
function(x, ...)UseMethod("l2boost")

#' l2boost implements the boosting using an l2-loss function
#'
#' l2boost is a fast implementation of Friedman's boosting algorithm with 
#' coordinate direction base learners and an l2-loss function.
#'
#' @title l2boost: linear regression boosting with an l2 loss function.
#' @param x design matrix of dimension n x p
#' @param y response variable of length n
#' @param formula an object of class \code{\link{formula}} 
#'     (or one that can be coerced to that class): a symbolic 
#'     description of the model to be fitted. The details of 
#'     model specification are given under "Details".
#' @param data an optional data frame, list or environment 
#'    (or object coercible by \code{\link{as.data.frame}} to 
#'    a data frame) containing the variables in the model. 
#'    If not found in data, the variables are taken from 
#'    "environment(formula)", typically the environment from 
#'    which \code{\link{lm}} is called.
#'
#' @param M number of steps to run boost algorithm (M >1)
#' @param nu l1 shrinkage parameter (0 < nu <= 1)
#' @param lambda l2 shrinkage parameter used for elastic net boosting (lambda > 0 || lambda = NULL)
#' @param type Choice of l2boost algorithm from "friedman", "discrete", "hybrid", "lars" 
#' @param qr.tolerance tolerance limit for use in \code{\link{qr.solve}} (default: 1e-30)
#' @param eps.tolerance dynamic step size lower limit (default: .Machine$double.eps)
#' @param trace show runtime messages (default: FALSE)
#' @param ... other arguments (currently unused)
#'
#' @return A "l2boost" object is returned, for which print, plot, predict,  and coef methods exist.
#'
#' @seealso \code{\link{print.l2boost}}, \code{\link{plot.l2boost}}, \code{\link{predict.l2boost}} methods of l2boost and \code{\link{cv.l2boost}}
#'
#' @references John Ehrlinger, Hemant Ishwaran (2012). Characterizing l2boosting. \emph{Annals of Statistics}, to appear. 
#'
#' @examples
#' data(diabetes)
#' par(mfrow=c(2,2))
#' attach(diabetes)
#' object <- l2boost(x,y, M=1000, nu=.01)
#' plot(object)
#' plot(object, type="coef")
#'
#' object2 <- l2boost(x,y,M=10000, nu=1.e-3) # increased shrinkage and number of iterations.
#' plot(object2)
#' plot(object2, type="coef")
#' detach(diabetes)
#'
#'
#' ## Compare l2boost and elasticNetBoosting using 10-fold CV
#'
#' par(mfrow=c(2,3))
#' dta <- elasticNetSim(n=100)
#' object3 <- l2boost(dta$x,dta$y,M=10000, nu=1.e-3, lambda=NULL) #  l2boost correlated data
#' cv.obj3<- cv.l2boost(dta$x,dta$y,M=10000, nu=1.e-3, lambda=NULL)
#' plot(object3)
#' plot(cv.obj3)
#' plot(coef(object3, m=cv.obj3$opt.step), cex=.5, ylab=expression(beta[i]))
#'     
#' object4 <- l2boost(dta$x,dta$y,M=10000, nu=1.e-3, lambda=.1) # elasticNet Boosting corellated data 
#' cv.obj4 <- cv.l2boost(dta$x,dta$y,M=10000, nu=1.e-3, lambda=.1) 
#' plot(object4)
#' plot(cv.obj4)
#' plot(coef(object4, m=cv.obj4$opt.step), cex=.5, ylab=expression(beta[i]))
#'
#' @rdname l2boost
#' @name l2boost
#' @S3method l2boost default
l2boost.default <-
function(x, y,
                    M = NULL, nu = 1e-4, lambda = NULL, trace = FALSE, 
                    type = c("friedman", "discrete", "hybrid", "lars"),
                    qr.tolerance = 1e-30,
                    eps.tolerance = .Machine$double.eps,
                    ...) {
  call<-match.call()
  # preliminary checks, set dimensions
  if (nu <= 0 || nu > 1) stop("nu set incorrectly:", nu, "\n")
  x.org <- x <- as.matrix(x)
  y.org <- y <- c(y)
  n.org <- n <- length(y)
  p <- ncol(x)
  if (n < 2)  stop("insufficient data, n =", n, "\n")
  
  # process the data
  ybar <- mean(y.org)
  y <- y - ybar
  cNames <- colnames(x)
  rownames(x) <- colnames(x) <- NULL
  x <- scale(x)/sqrt(n - 1)
  x.attr <- attributes(x)

  # remove x-columns with NA standardized values
  x.na <- sapply(1:p, function(j) {any(is.na(x[, j]))})
  if (any(x.na)) {
    x <- x[, !x.na]
    x.org <- x.org[, !x.na]
    p <- ncol(x)
    x <- scale(x)/sqrt(n - 1)
    x.attr <- attributes(x)
  }

  # elastic net modification to x, y
  enet <- (!is.null(lambda) && lambda > 0) 
  if (enet) {
    if (lambda < 0) stop("lambda must be positive")
    x <- rbind(x, diag(sqrt(lambda), p))/sqrt(1 + lambda)
    y <- c(y, rep(0, p))
    n <- length(y)
  }
  
  # decide which algorithm is to be applied
  type <- match.arg(type)
  TYPE <- switch(type,
                 friedman = "FRIEDMAN",
                 discrete = "DISCRETE",
                 hybrid   = "HYBRID",
                 lars     = "LARS")

  if (trace) cat(paste("implementing l2boost", TYPE, "..."), "\n")

  # special scenario for LARS
  if (TYPE == "LARS") {
    if (is.null(M)) M <- min(n, p - 1) else M <- min(M, p - 1)
  }
  else {
    if (is.null(M)) M <- min(n, p)
  }
  
  # initialize predictor
  # initialize beta 
  Fm <- rep(0, n)
  betam <- rep(0, p)
  Fm.path <- betam.path <- vector("list", length = (M + 1))
  Fm.path[[1]] <- Fm
  betam.path[[1]] <- betam

  # initialize (l,L,S) triplets
  # initialize rho entities
  # initialize x-correlation (corr) entities
  l.crit <- L.crit <- S.crit <- rep(0, M + 1)
  rhom.path <- vector(length = (M + 1), "list")
  corr.x <- vector(length = p, "list")
  VR <- rep(0, M)
  mjk <- vector(length = M, "list")
  stepSize <- vector(length = M, "list")
          
  # calculate all x_j^Ty: initializes rho
  # initialize rho-related quantities
  rho.m <- c(t(x) %*% y)
  l1 <- l.crit[1] <- resample(which.max.ind(abs(rho.m)))
  corr.x[[l1]] <- extract.corr(x, l1, enet, n.org)
  rhom.path[[1]] <- rho.m

  # cycling busting could terminate prior to the final r=M step
  early.terminate.flag <- FALSE

  #-----------------------------------------------------
  # Main entry point

  for (r in 1:M) {

    # assign lr
    lr <- l.crit[r]

    # extract the R_j,l correlation: only need new values
    if (r > 1 && (sum(lr == l.crit[1:(r - 1)]) == 0)) {
      corr.x[[lr]] <- extract.corr(x, lr, enet, n.org)
    }

    # What we do now depends upon TYPE 
   
    if (TYPE == "FRIEDMAN") {
      
      ##---------------FRIEDMAN-------------------
      ## original, bare-bones l2boost

      # get the next critical value
      # update the gradient
      # break ties randomly
      rho.update <- nu * rho.m[lr]
      rho.m.PlusOne <- rho.m - rho.update * corr.x[[lr]]
      lr.PlusOne <- resample(which.max.ind(abs(rho.m.PlusOne)))
      
      # trace
      if (trace) cat(r, lr, lr.PlusOne, nu, "\n")

      # updates
      L.crit[r + 1] <- 1
      l.crit[r + 1] <- lr.PlusOne 
      Fm <- Fm + rho.update * x[, lr]
      betam[lr] <- betam[lr] + rho.update
      rho.m <- rho.m.PlusOne

      #save mjk (methodology related)
      mjk[[r]] <- mstep.long(rho.m, corr.x[[lr]], lr, nu)

    }
    
    else if (TYPE == "DISCRETE") {

      ##---------------DISCRETE SOLUTION PATH-------------------
      ## unlike HYBRID:
      #  if nu is small you get one big step, followed by steps of size 1
      ## if nu is large you get steps of size 1

      # get the discrete path solution (uses random tie-breaking)
      path.solve <- get.discrete.solution(rho.m, corr.x, lr, nu)

      # (l,L,S) update; Vr
      l.crit[r + 1] <- path.solve$lr.PlusOne 
      L.crit[r + 1] <- path.solve$Lr 
      S.crit[r + 1] <- S.crit[r] + path.solve$Lr
      Vr <- (1 - (1 - nu)^path.solve$Lr)
      
      # trace
      if (trace) cat(r, lr, path.solve$lr.PlusOne, Vr, "\n")

      # updates
      rho.update <- Vr * rho.m[lr]
      Fm <- Fm + rho.update * x[, lr]
      betam[lr] <- betam[lr] + rho.update
      rho.m <- rho.m - rho.update * corr.x[[lr]]
      
      #save mjk (methodology related)
      mjk[[r]] <- path.solve$M.step

    }
   
    else if (TYPE == "HYBRID") {
      
      ##---------------HYBRID-------------------
      ## takes a big first step size even if nu is small
      ## ... works best if nu is moderate and not too small

      # get the dynamic path solution
      # breaks ties randomly using the gradient
      path.solve <- get.hybrid.solution(rho.m, corr.x, lr)

      # deal with ties

      #case 1, Lr>1 but dynamic ties (EXTREMELY RARE)
      if ((path.solve$nu.r.max > nu) & (length(path.solve$lr.PlusOne) > 1)) {
        cat("numerical issue with DYNAMIC ties: case1\n")                
        M.step <- floor(1 + log(1 - path.solve$nu.r)/log(1 - nu))
        lr.PlusOne <-  which.min.ind(M.step, lr)
        Lr <- unique(M.step[lr.PlusOne])
        lr.PlusOne <- break.ties(rho.m, corr.x[[lr]], lr, lr.PlusOne, Lr, nu)
      }
      #case 2, Lr=1, dynamic ties (RARE)
      else if ((path.solve$nu.r.max <= nu) & (length(path.solve$lr.PlusOne) > 1)) {
        cat("numerical issue with DYNAMIC ties: case2\n")
        lr.PlusOne <- break.ties(rho.m, corr.x[[lr]], lr, path.solve$lr.PlusOne, 1, nu)
      }
      else {
        #case 3, everything else (99.99999....% of the time)
        #(do nothing)
        lr.PlusOne <- path.solve$lr.PlusOne
      }
               
      # floor-bound on VR
      VR[r] <- max(path.solve$nu.r[lr.PlusOne], nu, na.rm = TRUE)
      
      # trace
      if (trace) cat(r, lr, lr.PlusOne, path.solve$nu.r[lr.PlusOne], VR[r], "\n")
      
      # updates
      l.crit[r + 1] <- lr.PlusOne 
      rho.update <- VR[r] * rho.m[lr]
      Fm <- Fm + rho.update * x[, lr]
      betam[lr] <- betam[lr] + rho.update
      rho.m <- rho.m - rho.update * corr.x[[lr]]
      
    }

    else if (TYPE == "LARS") {
     
      ##---------------LARS-l2boost-LIMIT-------------------
      # get the l2boost-lars-limit
      # first need to define the active set 
      # break step size ties randomly
      active.set <- l.crit[1:r]
      first.coord <- active.set[1]
      path.solve <- get.lars.solution(rho.m, corr.x, active.set, qr.tolerance, eps.tolerance)
      active.set.sign <- path.solve$active.set.sign
      VR[r] <- path.solve$nu.limit
      
      # trace
      if (trace) cat(r, lr, path.solve$lr.PlusOne, VR[r], "\n")

      # update step size
      stepSize[[r]] <- abs(path.solve$gmma)/sum(abs(path.solve$gmma), na.rm = TRUE)

      # updates
      l.crit[r + 1] <- path.solve$lr.PlusOne 
      rho.update <- VR[r] * rho.m[first.coord]
      Fm <- Fm + rho.update * rowSums(t(t(as.matrix(x[, active.set])) * active.set.sign * path.solve$gmma))
      betam[active.set] <- betam[active.set] + rho.update * active.set.sign * path.solve$gmma
      rho.m <- rho.m - rho.update * rowSums(sapply(1:length(active.set), function(j) {
        corr.x[[active.set[j]]] * path.solve$gmma[j] * active.set.sign[j]}))
    
    }

    
    # -----------common path updates---------------------
    # update Fm, betam and rhom paths (add ybar to Fm's path)
    Fm.path[[r + 1]] <- ybar + Fm 
    betam.path[[r + 1]] <- betam
    rhom.path[[r + 1]] <- rho.m

  }
    
  #---------check to see if early termination occurred during cycling busting----------
  if (early.terminate.flag) M <- r - 1

  #---------elastic net modification----------
  if (enet) {
    n.org <- length(y.org)
    Fm <- Fm[1:n.org]
    Fm.path <- lapply(1:(M + 1), function(r) {Fm.path[[r]][1:n.org]})
    betam <- betam * sqrt(1 + lambda)
    betam.path <- lapply(1:(M + 1), function(r) {betam.path[[r]] * sqrt(1 + lambda)})
  }    

  # ---------return the goodies------------------
  # trim M+1 indices
  # adjust Fm by adding ybar
  object <- list(
         call = call, type=type,
         l.crit = l.crit[1:M],
         L.crit = (if (TYPE == "FRIEDMAN" | TYPE == "DISCRETE") L.crit[-1] else VR[1:M]),
         S.crit = (if (TYPE == "DISCRETE") S.crit[-1] else NULL),
         mjk = (if (TYPE == "FRIEDMAN" | TYPE == "DISCRETE") mjk  else NULL),
         stepSize = (if (TYPE == "LARS") stepSize else NULL),        
         rhom.path = rhom.path[1:(M+1)],
         Fm = (ybar + Fm), Fm.path = Fm.path[1:(M+1)],
         betam = betam, betam.path = betam.path[1:(M+1)],
         x = x.org, x.attr = x.attr, x.na = x.na, names = cNames,
         y = y.org, ybar = ybar, nu=nu, lambda=lambda)
  class(object) <- c("l2boost", TYPE)
  invisible(object)
  
}

#' 
#' @rdname l2boost
#' @alias l2boost.formula
#' @S3method l2boost formula
l2boost.formula <-
function(formula, data=list(), ...){  
  mf <- model.frame(formula=formula, data=data)
  x<- model.matrix(attr(mf, "terms"), data=mf)
  y<-model.response(mf)

  est<- l2boost.default(x,y, ...)
  est$call<-match.call()
  est$formula <-formula
  invisible(est)
}
