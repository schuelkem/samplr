% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projectq3b.R
\name{find_quantile}
\alias{find_quantile}
\title{find_quantile}
\usage{
find_quantile(pdf, ...)
}
\arguments{
\item{pdf}{a function that is the pdf of the random variable.}

\item{...}{further arguments passed to or from other methods.}
}
\value{
a quantile in the support of the distribution
}
\description{
This function trials quantiles until one is found with positive density.
}
