#' Nomogram features using categorical and 1 numerical predictors
#'
#' An example of a data frame for `sample_features` argument in 
#' \code{\link{create_nomogram}} function, must only include all possible 
#' combinations of feature values, where one column is available for each 
#' feature.
#'
#' @format A data frame with 80 rows and 4 columns:
#' \describe{
#'.  \item{qsec}{A numerical predictor without decimal.}
#'   \item{cyl.6}{A categorical predictor with values of 0 and 1.}
#'   \item{cyl.8}{A categorical predictor with values of 0 and 1.}
#'   \item{vs.1}{A categorical predictor with values of 0 and 1.}
#' }
#'
#' @source Derived from \code{mtcars} for examples in this package.
#' @keywords dataset
#' @name nomogram_features2
'nomogram_features2'
