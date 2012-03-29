\name{mvnorm.l2boost}
\alias{mvnorm.l2boost}
\title{mvnorm.l2boost creates a data simulations for l2boost examples.}
\usage{
  mvnorm.l2boost(n = 100, p = 100, beta = NULL,
    which.beta = NULL, rho = 0)
}
\arguments{
  \item{n}{number of observations}

  \item{p}{number of coordinate directions in the design
  matrix}

  \item{beta}{a true beta vector of length p}

  \item{which.beta}{indicator vector for which beta
  coefficients to include in simulation}

  \item{rho}{between coordinate direction correlation
  coefficient}
}
\description{
  mvnorm.l2boost creates a data simulations for l2boost
  examples.
}
\examples{
n<- 100
p<- 100
beta <- c(rep(5,10), rep(0,p-10))
ortho.data <- mvnorm.l2boost(n, p, beta)
}

