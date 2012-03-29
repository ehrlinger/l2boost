
#' l2boost internal method to get discrete solution (critical value, critical point)
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
