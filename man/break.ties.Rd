\name{break.ties}
\alias{break.ties}
\title{break critical value ties.
This internal l2boost helper function determines which direction is more favorable when two directions have the same value of rho.m}
\usage{
  break.ties(rho.m, corr.x, lr, lr.PlusOne, Lr, nu)
}
\arguments{
  \item{rho.m}{gradient correlation of current direction at
  step m}

  \item{corr.x}{vector of gradient correlations for all
  candidate directions (including the current direction)}

  \item{lr}{index of the current direction}

  \item{lr.PlusOne}{index of the next candidate}

  \item{Lr}{length of current direction descent}

  \item{nu}{shrinkage parameter value}
}
\description{
  break critical value ties. This internal l2boost helper
  function determines which direction is more favorable
  when two directions have the same value of rho.m
}

