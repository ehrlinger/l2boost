#---------------------------------------------------------------------
# l2boost data simulations
#
# This is to encapsulate all simulation dataset in the application file
#---------------------------------------------------------------------

#' mvnorm.l2boost creates a data simulations for l2boost examples. This function reproduces the simulations from 
#' Ehrlinger and Ishwaran (2012)
#'
#' @param n number of observations
#' @param p number of coordinate directions in the design matrix
#' @param beta a true beta vector of length p
#' @param which.beta indicator vector for which beta coefficients to include in simulation
#' @param rho between coordinate direction correlation coefficient
#'
#' @examples
#' set.seed(1024)
#' n<- 100
#' p<- 100
#' beta <- c(rep(5,10), rep(0,p-10))
#' ortho.data <- mvnorm.l2boost(n, p, beta)
#' cbind(ortho.data$y[1:5],ortho.data$x[1:5,])
#' 
#' 
#' @references John Ehrlinger, Hemant Ishwaran (2012). Characterizing l2boosting. \emph{Annals of Statistics}, 40 (2), 1074-1101
#' 
#' @export mvnorm.l2boost
#' @importFrom MASS mvrnorm
mvnorm.l2boost <- function(n=100,p=100, beta=NULL, which.beta=NULL, rho=0){
  call<-match.call()
  if(rho == 0){
    sigma <- diag(1, p)
  }else{
    sigma <- matrix(rho, p, p)
    diag(sigma) <- 1
  }
  x <- mvrnorm(n, mu = rep(0, p), Sigma = sigma)
  
  beta.true <- rep(0, p)   
  if(is.null(beta)){
    if(is.null(which.beta)) beta.true[1:10] <- 5 else beta.true[which.beta] <- 5 
  }else if(length(beta) == p){
    beta.true = beta
  }else if(is.null(which.beta)){
    beta.true[1:length(beta)] <- beta
  }else{
    beta.true[which.beta] <- beta
  }
  y <- x %*% beta.true + rnorm(n)
  return(list(x=x, y=y, call =call))
}
