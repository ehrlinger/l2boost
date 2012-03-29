
#' limiting nu size 
nu.limit <-
function(rho.m, Rk, lr) {
    Dk <- rho.m/rho.m[lr]
    num <- Dk - Rk
    return(1 - abs(num)/(1 - Rk * sign(num)))
}
