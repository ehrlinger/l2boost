
#' l2boost internal method to  get hybrid solution (step size, critical value)
get.hybrid.solution <-
function(rho.m, corr.x, lr) {
  nu.r <- nu.limit(rho.m, corr.x[[lr]], lr)
  lr.PlusOne <- which.min.ind(nu.r, lr)
  nu.r.max <- max(nu.r[lr.PlusOne], na.rm = TRUE)
  return(list(nu.r = nu.r, lr.PlusOne = lr.PlusOne, nu.r.max = nu.r.max))
}
