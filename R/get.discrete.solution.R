
#' l2boost internal method to get discrete step sized solution (critical value, critical point)

#' @param rho.m vector of gradient corellations to this point (m)
#' @param corr.x correlation matrix
#' @param lr current step direction
#' @param nu l1 shrinkage parameter

get.discrete.solution <-
function(rho.m, corr.x, lr, nu) {
  # get the step size
  M.step <- mstep.long(rho.m, corr.x[[lr]], lr, nu)
  # determine the critical point and critical value
  # break-ties using the gradient-correlation
  lr.PlusOne <- which.min.ind(M.step, lr)
  Lr <- unique(M.step[lr.PlusOne])
  lr.PlusOne <- break.ties(rho.m, corr.x[[lr]], lr, lr.PlusOne, Lr, nu)
  return(list(Lr = Lr, lr.PlusOne = lr.PlusOne, M.step = M.step))
}
