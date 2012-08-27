\name{plot.lines}
\alias{plot.lines}
\title{plots.lines is used by \code{\link{plot.l2boost}} to the path lines (each j, against each r-step)}
\usage{
  plot.lines(xval = NULL, ind, path, l.crit, active = TRUE,
    col = NULL)
}
\arguments{
  \item{xval}{vector of x-values corresponding to the path
  y-values (default: NULL index of path)}

  \item{ind}{Coordinate of the path (for coloring
  individual paths)}

  \item{path}{Plot the path values along the y-axis}

  \item{l.crit}{change the color at the value of m=l.crit}

  \item{active}{active set coloring (default: TRUE)}

  \item{col}{vector of color values lnegth >= 1 (default:
  NULL use built in scheme)}
}
\description{
  plots.lines is used by \code{\link{plot.l2boost}} to the
  path lines (each j, against each r-step)
}

