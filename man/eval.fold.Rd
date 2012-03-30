\name{eval.fold}
\alias{eval.fold}
\title{core cv function}
\usage{
  eval.fold(k, K, all.folds, x, y, M, nu, lambda, trace,
    type)
}
\arguments{
  \item{k}{Index of this fold}

  \item{K}{Total number of folds to perform}

  \item{all.folds}{List of length K of observation indexes
  sorted into the K-fold data partitions}

  \item{x}{design matrix}

  \item{y}{response vector}

  \item{M}{Total number of l2boost iterations to perform}

  \item{nu}{l1 shrinkage parameter (0< nu <= 1)}

  \item{lambda}{l2 shrinkage parameter for elasticBoosting
  (lambda > 0 || lambda = NULL)}

  \item{trace}{Show fold progress information? (T||F)}

  \item{type}{Which l2boost algorithm?}
}
\description{
  core cv function
}
\seealso{
  \code{\link{l2boost}}
}

