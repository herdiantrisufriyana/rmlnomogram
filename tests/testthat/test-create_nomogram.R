library(testthat)
library(dplyr)

# Ensuring all possible combinations of one factor and one numeric column
sample_features <- expand.grid(
  feature1 = factor(c("low", "medium", "high")),  # Categorical predictor
  feature2 = c(1, 2, 3)                          # Numeric predictor
)

# Corresponding output for each combination
sample_output <- 
  data.frame(output = seq(0.1, 0.9, length.out = nrow(sample_features)))

# Feature explainability must align with sample_features in structure and order
feature_exp <- data.frame(
  feature1 = runif(nrow(sample_features), 0.1, 0.3),
  feature2 = runif(nrow(sample_features), 0.4, 0.6)
)

# Test if the function stops with incorrect input types
test_that("create_nomogram stops with incorrect input types", {
  expect_error(
    create_nomogram(123, sample_output), "sample_features must be a data frame"
  )
  expect_error(
    create_nomogram(sample_features, "output")
    , "sample_output must be a data frame"
  )
  expect_error(
    create_nomogram(sample_features, sample_output, feature_exp = "exp"), 
    "feature_exp must be a data frame if provided"
  )
})

# Test if the function stops with invalid data types or structures
test_that("create_nomogram validates data types and structures", {
  # Test for improper feature types
  wrong_features <- 
    expand.grid(
      feature1 = c("low", "medium", "high")
      , feature2 = c(1, 2, 3)
      , stringsAsFactors = FALSE
    )
  expect_error(
    create_nomogram(wrong_features, sample_output),
    "All columns in sample_features must be either factor or numeric."
  )
  
  # Test for column number and type constraints
  only_numerics <- expand.grid(feature1 = c(1, 2, 3), feature2 = c(4, 5, 6))
  only_factors <- 
    expand.grid(
      feature1 = factor(c("low", "medium", "high"))
      , feature2 = factor(c("yes", "no", "maybe"))
    )
  expect_error(
    create_nomogram(only_numerics, sample_output),
    "There must be at least one categorical predictor in sample_features."
  )
  expect_silent(
    create_nomogram(only_factors, sample_output)
  )
  expect_error(
    create_nomogram(
      sample_features, sample_output
      , feature_exp = data.frame(wrong = c(1, 2, 3))
    ),
    "feature_exp must have the same column names and order as sample_features."
  )
})

# Test for NA presence in any of the input data frames
test_that("create_nomogram handles NA values in inputs correctly", {
  # Setup for tests with NAs introduced in different data frames
  sample_features_na <- expand.grid(
    feature1 = factor(c("low", "medium", "high")),
    feature2 = c(1, 2, NA)  # NA in numeric column
  )
  
  sample_output_na <- data.frame(output = c(0.1, NA, 0.9))  # NA in output
  
  feature_exp_na <- data.frame(
    feature1 = c(0.1, 0.2, NA),  # NA in explainability data
    feature2 = c(0.4, 0.5, 0.6)
  )
  
  # Test NA in sample_features
  expect_error(
    create_nomogram(sample_features_na, sample_output),
    "sample_features must not contain any NA values."
  )
  
  # Test NA in sample_output
  expect_error(
    create_nomogram(
      expand.grid(
        feature1 = factor(c("low", "medium", "high")), feature2 = c(1, 2, 3)
      ),
      sample_output_na
    ),
    "sample_output must not contain any NA values."
  )
  
  # Test NA in feature_exp
  expect_error(
    create_nomogram(
      expand.grid(
        feature1 = factor(c("low", "medium", "high")), feature2 = c(1, 2, 3)
      ),
      data.frame(output = c(0.1, 0.5, 0.9)),
      feature_exp_na
    ),
    "feature_exp must not contain any NA values."
  )
})

# Test function with correct inputs
test_that("create_nomogram returns a ggplot object with correct inputs", {
  result <- create_nomogram(sample_features, sample_output, prob = TRUE)
  expect_true(
    inherits(result, "ggplot"), "The output should be a ggplot object"
  )
})

# Additional tests to ensure feature structure constraints are respected
test_that("create_nomogram handles number and type of predictors correctly", {
  two_numerics <- expand.grid(
    feature1 = factor(c("low", "medium", "high")),
    feature2 = c(1, 2, 3),
    feature3 = c(4, 5, 6)
  )
  valid_features <- expand.grid(
    feature1 = factor(c("low", "medium", "high")),
    feature2 = c(1, 2, 3)
  )
  expect_error(
    create_nomogram(two_numerics, sample_output),
    "There must be no more than one numerical predictor in sample_features."
  )
  expect_silent(
    create_nomogram(valid_features, sample_output)
  )
})

test_that("sample_features includes all possible combinations", {
  # Create a sample_features dataframe with incomplete combinations
  incomplete_features <- data.frame(
    feature1 = factor(c("low", "medium")), # Missing 'high'
    feature2 = c(1, 2) # Correct
  )
  
  complete_features <- expand.grid(
    feature1 = factor(c("low", "medium", "high")),
    feature2 = c(1, 2)
  )
  
  # Expect error for incomplete combinations
  expect_error(
    create_nomogram(incomplete_features, data.frame(output = c(0.5, 0.6))),
    paste0(
      "sample_features must include all possible combinations of the feature "
      , "values."
    )
  )
  
  # Should not throw an error for complete combinations
  expect_silent(
    create_nomogram(
      complete_features, data.frame(output = runif(nrow(complete_features)))
    )
  )
})