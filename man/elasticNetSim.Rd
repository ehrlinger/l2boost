\name{elasticNetSim}
\alias{elasticNetSim}
\title{A blocked correlated data simulation.}
\usage{
  elasticNetSim(n, p = 40, p0 = 15, signal = 3,
    sigma = sqrt(0.01), beta.true = NULL)
}
\arguments{
  \item{n}{number of observations}

  \item{p}{number of coordinate directions in the design
  matrix (default 40)}

  \item{p0}{number of signal coordinate directions in the
  design matrix (default 15)}

  \item{signal}{number of signal groups (default 3)}

  \item{sigma}{within group correlation coefficient
  (default sqrt(0.01))}

  \item{beta.true}{specify the true simulation parameters.
  (default NULL = generated from other arguments)}
}
\value{
  list of \itemize{ \item x simulated design matrix \item y
  simulated response vector \item beta.true true beta
  parameters used to create the simulation }
}
\description{
  Creates a data simulation of \emph{n} observations with
  \emph{signal} groups of (\emph{p0/signal}) signal
  variables and (\emph{p-p0}) noise variables. Random noise
  is added to all columns. The default values, with
  \emph{n=100} create the simulation of Zou and Hastie
  (2005).
}
\examples{
#--------------------------------------------------------------------------
# Example: Elastic net simulation
#
# For elastic net simulation data, see Zou, H. and Hastie, T. (2005)
# Regularization and variable selection via the elastic net J. R. Statist. Soc. B
# , 67, Part 2, pp. 301-320
  # Set the RNG seed to create a reproducible simulation
  set.seed(432) # Takes an integer argument

  # Creata simulation with 100 observations.
  dta <- elasticNetSim(n=100)

  # The simulation contains a design matrix x, and response vector y
  dim(dta$x)
  length(dta$y)
  print(dta$x[1:5,])
}
\references{
  Zou, H. and Hastie, T. (2005) Regularization and variable
  selection via the elastic net \emph{J. R. Statist. Soc.
  B}, 67, Part 2, pp. 301-320
}

