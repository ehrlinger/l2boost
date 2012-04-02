\name{predict.l2boost}
\alias{predict.l2boost}
\title{predict method for l2boost objectects.}
\usage{
  predict.l2boost(object, xnew = NULL,
    type = c("fit", "coef"), newdata = xnew, ...)
}
\arguments{
  \item{object}{an l2boost objectect}

  \item{xnew}{a new design matrix to fit with the l2boost
  object}

  \item{newdata}{a new design matrix to fit with the
  l2boost object}

  \item{type}{fit returns the predicted values coef returns
  the model coefficients}

  \item{...}{other arguments (currently not used)}
}
\description{
  predict method for l2boost objectects.
}

