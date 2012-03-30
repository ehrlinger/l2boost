\name{extract.corr}
\alias{extract.corr}
\title{extract correlations: uses a smart reduction for elastic net modification}
\usage{
  extract.corr(x, l, enet, n.org)
}
\arguments{
  \item{x}{design matrix (possibly augmented for
  elasticBoosting)}

  \item{l}{currently selected direction}

  \item{enet}{elasticBoost indicator, requires since we
  only scale the non-augmented design matrix}

  \item{n.org}{length of the non-augmented design matrix}
}
\description{
  extract correlations: uses a smart reduction for elastic
  net modification
}

