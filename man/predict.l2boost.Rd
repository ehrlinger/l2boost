\name{predict.l2boost}
\alias{predict.l2boost}
\title{predict method for l2boost objects.}
\usage{
  predict.l2boost(obj, xnew = NULL,
    type = c("fit", "coef"), newdata = xnew)
}
\arguments{
  \item{obj}{an l2boost object}

  \item{xnew}{a new design matrix to fit with the l2boost
  obj}

  \item{type}{fit returns the predicted values coef returns
  the model coefficients}
}
\description{
  predict method for l2boost objects.
}

