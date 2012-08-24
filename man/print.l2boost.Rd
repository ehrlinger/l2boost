\name{print.l2boost}
\alias{print.l2boost}
\title{print method for l2boost objects}
\usage{
  \method{print}{l2boost} (x, ...)
}
\arguments{
  \item{x}{an l2boost object}

  \item{...}{other arguments (not used)}
}
\description{
  print method for l2boost objects
}
\examples{
\dontrun{
#--------------------------------------------------------------------------
# Example 1: Diabetes
#
# See Efron B., Hastie T., Johnstone I., and Tibshirani R.
# Least angle regression. Ann. Statist., 32:407-499, 2004.
data(diabetes)

object <- l2boost(diabetes$x,diabetes$y, M=1000, nu=.01)
print(object)
}
}

