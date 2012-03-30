
#' limiting nu size 

#' @param rho.m vector of stagewise regression parameters
#' @param Rk correlation coefficient of current and candidate coordinate directions
#' @param lr candidate direction index
nu.limit <-
function(rho.m, Rk, lr) {
    Dk <- rho.m/rho.m[lr]
    num <- Dk - Rk
    return(1 - abs(num)/(1 - Rk * sign(num)))
}
