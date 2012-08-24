\name{plot.l2boost}
\alias{plot.l2boost}
\title{plot method for l2boost objects}
\usage{
  \method{plot}{l2boost} (x, type = c("rho", "coef"),
    standardize = TRUE, active.set = NULL,
    xvar = c("step", "norm"), x.lab = NULL, y.lab = NULL,
    trim = TRUE, clip = NULL, col = NULL, ylim = NULL,
    xlim = NULL, ...)
}
\arguments{
  \item{x}{l2boost or cv.l2boost object}

  \item{type}{which type of plot. \emph{rho} shows gradient
  correlation, \emph{coef} regression (beta) coefficients}

  \item{standardize}{Should we plot standardized gradient
  correlation (default: TRUE)}

  \item{active.set}{Vector of indices of the coordinates
  for highlighting with color=col (default: NULL shows all
  active coordinates)}

  \item{xvar}{what measure do we plot on the x-axis?
  \emph{step} plots the step m, \emph{norm} plots the
  normalized distance (1-nu)^(m-1)}

  \item{x.lab}{specific x-axis label (NULL results in
  default value depending on xvar)}

  \item{y.lab}{specific y-axis label (NULL results in
  default value depending on type)}

  \item{trim}{(default: TRUE)}

  \item{clip}{Do we want to c}

  \item{col}{Color to highlight active.set coordinates
  (NULL indicates default all active set at step M in blue,
  changes to red after selection}

  \item{ylim}{Control plotted y-values (default: NULL for
  auto range)}

  \item{xlim}{Control plotted x-values (default: NULL for
  auto domain )}

  \item{...}{other arguments passed to plot functions}
}
\value{
  \code{NULL}
}
\description{
  plot method for l2boost objects
}
\seealso{
  \code{\link{l2boost}}, \code{\link{print.l2boost}},
  \code{\link{predict.l2boost}} methods of l2boost and
  \code{\link{cv.l2boost}}
}

