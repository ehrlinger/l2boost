\name{mvnorm.l2boost}
\alias{mvnorm.l2boost}
\title{Data simulations from Ehrlinger and Ishwaran (2012).}
\usage{
  mvnorm.l2boost(n = 100, p = 100, beta = NULL,
    which.beta = NULL, rho = 0)
}
\arguments{
  \item{n}{number of observations}

  \item{p}{number of coordinate directions in the design
  matrix}

  \item{beta}{a "true" beta vector of length p}

  \item{which.beta}{indicator vector for which beta
  coefficients to include in simulation}

  \item{rho}{between coordinate direction correlation
  coefficient}
}
\value{
  \itemize{ \item call Matched function call \item x design
  matrix of size \emph{n} x \emph{p} \item y response
  vector of length \emph{n} }
}
\description{
  Simulated data from a multivariate normal.
}
\examples{
#--------------------------------------------------------------------------
# Example: Multivariate normal data simulation

# Create a (reproducable) data set of size 100 x 100
set.seed(1024)
n<- 100
p<- 100

# Set 10 signal variables using a uniform beta=5, the remaining (p-10)=90 are
# set to zero indicating random noise.
beta <- c(rep(5,10), rep(0,p-10))

# Example with orthogonal design matrix columns (orthogonal + noise)
ortho.data <- mvnorm.l2boost(n, p, beta)
cbind(ortho.data$y[1:5],ortho.data$x[1:5,])

# Example with correlation between design matrix columns
corr.data <- mvnorm.l2boost(n, p, beta, rho=0.65)
cbind(corr.data$y[1:5],corr.data$x[1:5,])
}
\references{
  Ehrlinger J., and Ishwaran H. (2012). "Characterizing
  l2boosting" \emph{Ann. Statist.}, 40 (2), 1074-1101
}

