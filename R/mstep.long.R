
#' l2boost function to determine step size for long descent
#' |Dk| must be less than or equal to 1; force it otherwise
#' enforce step=infty when dk=Rk=1
#'
mstep.long <-
function(rho.m, Rk, lr, nu) {
    Dk <- pmin(pmax(rho.m/rho.m[lr], -1, na.rm = TRUE), 1)
    num <- Dk - Rk
    nu.r <- abs(num)/(1 - Rk * sign(num))
    mjk <- floor(1 + log(nu.r)/log(1 - nu))
    mjk[Dk == 1 & Rk == 1] <- Inf
    mjk
}
