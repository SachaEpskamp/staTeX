\name{swp}
\alias{swp}
\title{Return statistic and p values in LaTeX format}
\usage{
  swp(statistic, value, pval, df = NULL, digits = 3,
    sig = FALSE, alpha = c(0.05, 0.01, 0.001),
    parantheses = TRUE)
}
\arguments{
  \item{statistic}{Name of the statistic used}

  \item{value}{Value of the statistic}

  \item{pval}{p-value}

  \item{df}{The degrees of freedom. Can be a vector with
  multiple degrees of freedom}

  \item{digits}{The number of digits to be printed}

  \item{sig}{Logical, if TRUE not the real value but only
  the value compared to levels of 'alpha' is printed. This
  means that if p is smaller than one of the levels of
  'alpha' than that is returned (e.g. p<0.05) else ns is
  returned.}

  \item{alpha}{A vector specifying the significance levels
  used by 'sig'}

  \item{parantheses}{Logical, should parantheses be
  printed?}
}
\description{
  This is the function used by \code{\link{swst}} to
  generate the LaTeX code.
}
\author{
  Sacha Epskamp
}
\seealso{
  \code{\link{swst}} \code{\link{swstNP}}
}

