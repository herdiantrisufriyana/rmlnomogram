# Use this to avoid R CMD check notes about undefined global variables
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("output", "term", "feature", "value", "magnitude",
                           "norm_magnitude", "i2", "factor_group", "variable", 
                           "pred", "variable_num", "position"))
}