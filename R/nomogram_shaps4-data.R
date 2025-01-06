#' Nomogram SHAP values using categorical and 1 numerical predictors and 
#' numerical outcome
#'
#' An example of a data frame for `feature_exp` argument in 
#' \code{\link{create_nomogram}} function, must only include feature 
#' explainability value per sample (i.e., SHAP value), where one column is 
#' available for each feature.
#'
#' @format A data frame with 80 rows and 4 columns:
#' \describe{
#'   \item{cyl.6}{A predictor with SHAP values.}
#'   \item{cyl.8}{A predictor with SHAP values.}
#'   \item{qsec}{A predictor with SHAP values.}
#'   \item{vs.1}{A predictor with SHAP values.}
#' }
#'
#' @source Computed by iml from a caret randomforest model using categorical 
#' and 1 numerical predictors for examples in this package.
#' @keywords dataset
#' @name nomogram_shaps4
'nomogram_shaps4'
