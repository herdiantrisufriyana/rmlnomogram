% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nomogram_features3-data.R
\docType{data}
\name{nomogram_features3}
\alias{nomogram_features3}
\title{Nomogram features using categorical predictors}
\format{
A data frame with 16 rows and 4 columns:
\describe{
\item{cyl.6}{A categorical predictor with values of 0 and 1.}
\item{cyl.8}{A categorical predictor with values of 0 and 1.}
\item{qsec.1}{A categorical predictor with values of 0 and 1.}
\item{vs.1}{A categorical predictor with values of 0 and 1.}
}
}
\source{
Derived from \code{mtcars} for examples in this package.
}
\usage{
nomogram_features3
}
\description{
An example of a data frame for \code{sample_features} argument in
\code{\link{create_nomogram}} function, must only include all possible
combinations of feature values, where one column is available for each
feature.
}
\keyword{dataset}
