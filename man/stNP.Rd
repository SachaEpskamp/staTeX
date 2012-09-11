\name{stNP}
\alias{stNP}
\title{LaTeX code for statistical reference (no paranthesis version)}
\usage{
  stNP(x, ...)
}
\arguments{
  \item{x}{An input object. Currently supported are a
  numerical vector and objects of classes "htest"
  (depending on statistic), "lm", "aov" and "anova".}

  \item{\dots}{Arguments sent to \code{\link{staTeX}}}
}
\value{
  A strings containing LaTeX code, or a named vector of
  strings containing LaTeX code
}
\description{
  The same as \code{\link{st}} except that no parantheses
  are printed
}

\author{
  Sacha Epskamp (mail@sachaepskamp.com)
}
\seealso{
  \code{\link{st}} \code{\link{staTeX}}
}

