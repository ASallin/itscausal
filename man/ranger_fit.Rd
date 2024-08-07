% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DEPRECATED_ranger_fit.r
\name{ranger_fit}
\alias{ranger_fit}
\title{ranger_fit}
\usage{
ranger_fit(x, y, args = list())
}
\arguments{
\item{x}{Matrix of covariates (number of observations times number of covariates matrix)}

\item{y}{vector of outcomes (no df format)}

\item{args}{List of arguments passed to  \code{\link{ranger}}}
}
\value{
An object with S3 class "ranger"
}
\description{
This function estimates random forest lasso regression based on the \code{\link{ranger}} package
}
\keyword{internal}
