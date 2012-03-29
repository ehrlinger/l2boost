\name{l2boost}
\alias{l2boost}
\title{l2boost: linear regression boosting with an l2 loss function.}
\usage{
  l2boost(x, ...)
}
\arguments{
  \item{x}{design matrix of dimension n x p}

  \item{y}{response variable of length n}

  \item{M}{number of steps to run boost algorithm M >= p}

  \item{nu}{l1 shrinkage parameter 0 < nu <= 1}

  \item{lambda}{l2 shrinkage parameter used for elastic net
  boosting 0 < lambda}

  \item{type}{= c("friedman", "discrete", "hybrid",
  "lars")}

  \item{qr.tolerance}{= 1e-30}

  \item{eps.tolerance}{= .Machine$double.eps}

  \item{trace}{= FALSE}
}
\description{
  l2boost implements the boosting using an l2-loss function
}
\details{
  l2boost is a fast implementation of Friedman's boosting
  algorithm with coordinate direction base learners and an
  l2-loss function.
}

