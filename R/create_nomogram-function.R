#' Construct nomogram for a machine learning model
#'
#' This function constructs a nomogram for either binary or continuous outcomes 
#' based on provided sample features and outputs. It can also incorporate 
#' feature explainability values, such as SHAP values.
#'
#' @param sample_features A data frame of feature values where each column 
#' represents a feature. The data frame must contain all possible combinations 
#' of feature values. There must be at least one categorical predictor and no 
#' more than one numerical predictor. Only factor and numeric data types are 
#' allowed. The column name 'output' is not allowed. Must not contain any NA 
#' values.
#' @param sample_output A data frame with one column 'output' containing 
#' numeric values for either the predicted probabilities (for binary outcomes) 
#' or estimated values (for continuous outcomes). Must not contain any NA 
#' values.
#' @param feature_exp Optional data frame containing feature explainability 
#' values (e.g., SHAP values) with one column for each feature. The structure 
#' must match `sample_features` in terms of column names. Each column must 
#' contain numeric values. Must not contain any NA values.
#' @param threshold A numeric scalar between 0 and 1, used to define the 
#' threshold for classifying predicted probabilities into binary outcomes. A 
#' sample is predicted positive if the predicted probability is equal or 
#' greater than this threshold.
#' @param prob A logical scalar indicating if the predicted probabilities 
#' should be shown in the nomogram.
#' @param est A logical scalar indicating if the estimated values should be 
#' shown in the nomogram.
#' @param verbose A logical scalar indicating whether to show a progress bar if 
#' it is required.
#'
#' @return A ggplot object representing the nomogram.
#'
#' @keywords ml-nomogram
#'
#' @export
#'
#' @importFrom dplyr mutate_if select mutate filter select join_by group_by 
#' ungroup arrange select_if arrange_at n case_when left_join all_of summarize
#' @importFrom purrr imap reduce
#' @importFrom broom tidy
#' @importFrom stats reorder median
#' @importFrom ggplot2 ggplot geom_tile aes facet_grid scale_x_discrete ylab
#' scale_y_continuous scale_fill_discrete theme element_blank element_text 
#' geom_vline geom_path scale_x_continuous scale_color_discrete 
#' scale_y_discrete element_rect unit
#' @importFrom ggpubr ggarrange
#' @importFrom stringr str_detect str_count
#' @importFrom tidyr spread gather separate_rows
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
#' @examples
#'
#' # Binary outcome (or class-wise multinomial outcome)
#' 
#' ## 1 - Categorical predictors and binary outcome without probability
#' data(nomogram_features)
#' data(nomogram_outputs)
#' create_nomogram(nomogram_features, nomogram_outputs)
#'
#' ## 2 - Categorical predictors and binary outcome with probability
#' create_nomogram(nomogram_features, nomogram_outputs, prob = TRUE)
#' 
#' data(nomogram_shaps)
#' create_nomogram(
#'   nomogram_features, nomogram_outputs, nomogram_shaps
#'   , prob = TRUE
#' )
#'
#' ## 3 - Categorical and 1 numerical predictors and binary outcome with probability
#' data(nomogram_features2)
#' data(nomogram_outputs2)
#' create_nomogram(nomogram_features2, nomogram_outputs2, prob = TRUE)
#'
#' data(nomogram_shaps2)
#' create_nomogram(
#'   nomogram_features2, nomogram_outputs2, nomogram_shaps2
#'   , prob = TRUE
#' )
#' 
#' # Continuous outcome
#' 
#' ## 4 - Categorical predictors and continuous outcome
#' data(nomogram_features3)
#' data(nomogram_outputs3)
#' create_nomogram(nomogram_features3, nomogram_outputs3, est = TRUE)
#'
#' data(nomogram_shaps3)
#' create_nomogram(
#'   nomogram_features3, nomogram_outputs3, nomogram_shaps3
#'   , est = TRUE
#' )
#' 
#' ## 5 - Categorical and 1 numerical predictors and continuous outcome
#' data(nomogram_features4)
#' data(nomogram_outputs4)
#' create_nomogram(nomogram_features4, nomogram_outputs4, est = TRUE)
#'
#' data(nomogram_shaps4)
#' create_nomogram(
#'   nomogram_features4, nomogram_outputs4, nomogram_shaps4
#'   , est = TRUE
#' )

create_nomogram <- function(sample_features, sample_output, feature_exp = NULL, threshold = 0.5, prob = FALSE, est = FALSE, verbose = FALSE){
  # Input validation
  ## Input validation for sample_features
  if (!is.data.frame(sample_features)) {
    stop("sample_features must be a data frame.")
  }
  if (ncol(sample_features) < 1) {
    stop("sample_features must include at least one column.")
  }
  if (
    any(!sapply(sample_features, function(x) is.factor(x) || is.numeric(x)))
  ) {
    stop("All columns in sample_features must be either factor or numeric.")
  }
  
  ## Ensure there is at least one factor and at most one numeric column
  num_factors <- sum(sapply(sample_features, is.factor))
  num_numerics <- sum(sapply(sample_features, is.numeric))
  if (num_factors < 1) {
    stop(
      "There must be at least one categorical predictor in sample_features."
    )
  }
  if (num_numerics > 1) {
    stop(
      "There must be no more than one numerical predictor in sample_features."
    )
  }
  
  ## Check for NA in data frames
  if (anyNA(sample_features)) {
    stop("sample_features must not contain any NA values.")
  }
  if (anyNA(sample_output)) {
    stop("sample_output must not contain any NA values.")
  }
  if (!is.null(feature_exp) && anyNA(feature_exp)) {
    stop("feature_exp must not contain any NA values.")
  }
  
  # Calculate the number of expected combinations
  expected_combinations <- prod(sapply(sample_features, function(col) {
    if (is.factor(col)) {
      return(length(levels(col)))
    } else {
      return(length(unique(col)))
    }
  }))
  
  if (nrow(sample_features) != expected_combinations) {
    stop(
      paste0(
        "sample_features must include all possible combinations of the "
        , "feature values."
      )
    )
  }
  
  ## Validate sample_output
  if (!is.data.frame(sample_output)) {
    stop("sample_output must be a data frame.")
  }
  if (!identical(names(sample_output), "output")) {
    stop("sample_output must contain only one column named 'output'.")
  }
  if (!is.numeric(sample_output$output)) {
    stop(
      "The 'output' column in sample_output must contain only numeric values."
    )
  }
  
  ## Validation for feature_exp
  if (!is.null(feature_exp)) {
    if (!is.data.frame(feature_exp)) {
      stop("feature_exp must be a data frame if provided.")
    }
    if (
      !(all(names(feature_exp) %in% names(sample_features))
        & all(names(sample_features) %in% names(feature_exp))
      )
    ) {
      stop(
        paste0(
          "feature_exp must have the same column names and order as "
          , "sample_features."
        )
      )
    }
    if (any(!sapply(feature_exp, is.numeric))) {
      stop("All columns in feature_exp must contain numeric values.")
    }
  }
  
  ## Additional checks for 'output' column avoidance in feature data frames
  if (
    "output" %in% names(sample_features) 
    || (!is.null(feature_exp) && "output" %in% names(feature_exp))
  ) {
    stop(
      paste0(
        "Neither sample_features nor feature_exp should contain a column "
        , "named 'output'."
      )
    )
  }
  
  ## Threshold validation
  if (
    !is.numeric(threshold) 
    || length(threshold) != 1 
    || threshold < 0 
    || threshold > 1
  ) {
    stop("threshold must be a numeric value between 0 and 1.")
  }
  
  ## Logical parameters validation
  if (!is.logical(prob) || length(prob) != 1) {
    stop("prob must be a single logical value.")
  }
  if (!is.logical(est) || length(est) != 1) {
    stop("est must be a single logical value.")
  }
  if (!is.logical(verbose) || length(verbose) != 1) {
    stop("verbose must be a single logical value.")
  }
  
  # Function implementation
  ## Change feature categories features to binaries
  sample_features0 <- sample_features
  sample_features <-
    mutate_if(sample_features, is.factor, \(x) as.numeric(x) - 1)
  
  ## Combine feature values with outputs
  sample_data <- cbind(sample_output, sample_features)
  
  if(is.null(feature_exp)){
    feature_exp_input_null <- TRUE
    
    feature_exp <-
      sample_data |>
      select(-output) |>
      colnames()
    
    feature_exp <-
      `names<-`(feature_exp, feature_exp) |>
      imap(
        ~ lm(as.formula(paste0("output~", .x)), data = sample_data) |>
          tidy()
      ) |>
      imap(~ mutate(.x, feature = .y, exp = exp(estimate + std.error))) |>
      reduce(rbind) |>
      filter(term != "(Intercept)") |>
      select(feature, exp) |>
      mutate(feature = factor(feature, feature_exp)) |>
      spread(feature, exp)
    
    feature_exp <-
      data.frame(i = seq(nrow(sample_data))) |>
      cbind(feature_exp) |>
      select(-i)
  }else{
    feature_exp_input_null <- FALSE
  }
  
  ## Combine data and sort feature values sorted by exp normalized magnitudes
  nomogram_data <-
    sample_data |>
    mutate(i = seq(nrow(sample_data))) |>
    gather(feature, value, -i, -output) |>
    left_join(
      feature_exp |>
        mutate(i = seq(nrow(feature_exp))) |>
        gather(feature, exp, -i) 
      , by = join_by(i, feature)
    ) |>
    group_by(feature) |>
    mutate(magnitude = max(exp)) |>
    ungroup() |>
    mutate(
      norm_magnitude =
        (magnitude - min(magnitude)) / (max(magnitude) - min(magnitude))
    ) |>
    mutate(feature = reorder(feature, norm_magnitude)) |>
    arrange(i, feature) |>
    mutate(factor_group = NA)
  
  sorted_factor_names <-
    levels(nomogram_data$feature)[
      levels(nomogram_data$feature)
      %in% colnames(select_if(sample_features0, is.factor))
    ]
  
  if(prob | est){
    nomogram_data_factor_spread <-
      nomogram_data |>
      filter(feature %in% sorted_factor_names) |>
      select(i, feature, value) |>
      spread(feature, value) |>
      arrange_at(sorted_factor_names) |>
      mutate(i2 = seq(n()))
    
    num_predictor <- sum(sapply(sample_features0, is.numeric))
    cat_predictor <- sum(sapply(sample_features0, is.factor))
    
    if(num_predictor == 1){
      nomogram_data_factor_group <-
        nomogram_data_factor_spread |>
        select(-i, -i2) |>
        unique() |>
        mutate(factor_group = rev(seq(n())))
      
      nomogram_data_resorted <-
        nomogram_data |>
        select(-factor_group) |>
        left_join(
          nomogram_data_factor_spread |>
            left_join(
              nomogram_data_factor_group
              , by =
                setdiff(colnames(nomogram_data_factor_spread), c("i", "i2"))
            ) |>
            select(i, i2, factor_group)
          , by = join_by(i)
        ) |>
        mutate(i = i2) |>
        select(-i2) |>
        arrange(i, feature, value)
    }else{
      nomogram_data_resorted <-
        nomogram_data |>
        select(-factor_group) |>
        left_join(
          select(nomogram_data_factor_spread, i, i2)
          , by = join_by(i)
        ) |>
        mutate(i = i2) |>
        select(-i2) |>
        arrange(i, feature, value)
    }
    
    factor_grid_plot <-
      nomogram_data_resorted |>
      filter(feature %in% sorted_factor_names)
    
    if(num_predictor == 1){
      factor_grid_plot <- unique(mutate(factor_grid_plot, i = 1))
    }
    
    factor_grid_plot <-
      factor_grid_plot |>
      mutate(
        value =
          case_when(
            value == 0 ~ "Negative"
            , value == 1 ~ "Positive"
            , TRUE ~ "Missing"
          )
        , value = factor(value, c("Negative", "Positive"))
      ) |>
      ggplot(aes(feature, i)) +
      geom_tile(aes(fill = value), color = "white")
    
    if(num_predictor == 1){
      factor_grid_plot <-
        factor_grid_plot +
        facet_grid(factor_group ~ ., scales = "free_y", space = "free_y")
    }
    
    factor_grid_plot <- factor_grid_plot + scale_x_discrete("Predictor")
    
    if(num_predictor != 1){
      factor_grid_plot <-
        factor_grid_plot +
        scale_y_continuous(
          breaks = seq(max(nomogram_data$i))
          , limits = c(min(nomogram_data$i) - 0.5, max(nomogram_data$i) + 0.5)
        )
    }
    
    factor_grid_plot <-
      factor_grid_plot +
      scale_fill_discrete("Value") +
      theme(
        panel.grid.minor.y = element_blank()
        , axis.title.x = element_text(size = 8)
        , axis.text.x = element_text(size = 7)
        , axis.title.y = element_blank()
        , axis.text.y = element_blank()
        , axis.ticks.y = element_blank()
        , legend.position = "left"
      )
    
    if(num_predictor == 1){
      factor_grid_plot <-
        factor_grid_plot +
        theme(strip.text.y = element_blank())
    }
    
    if(num_predictor == 1){
      output_plot <-
        nomogram_data_resorted |>
        filter(feature %in% colnames(select_if(sample_features0, is.numeric))) |>
        ggplot(aes(output, value))
    }else{
      output_plot <- ggplot(nomogram_data_resorted, aes(output, i))
    }
    
    if(!est){
      output_plot <- output_plot + geom_vline(xintercept = threshold, lty = 2)
    }
    
    output_plot <- output_plot + geom_path()
    
    if(num_predictor == 1){
      output_plot <-
        output_plot +
        facet_grid(factor_group ~ ., scales = "free_y", space = "free_y")
    }
    
    output_plot <-
      output_plot + scale_x_continuous(ifelse(est, "Outcome", "Outcome ->"))
    
    if(num_predictor == 1){
      output_plot <-
        output_plot +
        ylab(colnames(select_if(sample_features0, is.numeric)))
    }else{
      output_plot <-
        output_plot +
        scale_y_continuous(
          breaks = seq(max(nomogram_data$i))
          , limits = c(min(nomogram_data$i) - 0.5, max(nomogram_data$i) + 0.5)
        )
    }
    
    output_plot <-
      output_plot +
      theme(
        axis.title.x = element_text(size = 8)
        , axis.text.x = element_text(size = 7)
      )
    
    if(num_predictor == 1){
      output_plot <- output_plot + theme(strip.text.y = element_blank())
    }else{
      output_plot <-
        output_plot +
        theme(
          panel.grid.minor.y = element_blank()
          , axis.title.y = element_blank()
          , axis.text.y = element_blank()
          , axis.ticks.y = element_blank()
        )
    }
    
    if(feature_exp_input_null){
      ggarrange(
        factor_grid_plot
        , output_plot
        , nrow = 1, ncol = 2
        , widths = c(3, 2)
      )
    }else{
      exp_plot <- nomogram_data_resorted
      
      if(num_predictor == 1){
        exp_plot <-
          exp_plot |>
          left_join(
            nomogram_data_resorted |>
              filter(
                feature %in% colnames(select_if(sample_features0, is.numeric))
              ) |>
              select(i, i2 = value) |>
              unique()
            , by = join_by(i)
          ) |>
          mutate(i = i2) |>
          select(-i2) |>
          arrange(i, feature, value) |>
          mutate(factor_group = ifelse(is.na(factor_group), 1, factor_group))
      }
      
      exp_plot <-
        exp_plot |>
        ggplot(aes(exp, i)) +
        geom_vline(xintercept = 0, lty = 2) +
        geom_path(aes(color = feature), na.rm = TRUE)
      
      if(num_predictor == 1){
        exp_plot <- exp_plot + facet_grid(factor_group ~ .)
      }
      
      exp_plot <- exp_plot + scale_x_continuous("Impact on outcome ->")
      
      if(num_predictor != 1){
        exp_plot <-
          exp_plot +
          scale_y_continuous(
            breaks = seq(max(nomogram_data$i))
            , limits = c(min(nomogram_data$i) - 0.5, max(nomogram_data$i) + 0.5)
          )
      }
      
      exp_plot <-
        exp_plot +
        scale_color_discrete("Predictor") +
        theme(
          axis.title.x = element_text(size = 8)
          , axis.text.x = element_text(size = 7)
          , axis.title.y = element_blank()
          , axis.text.y = element_blank()
          , axis.ticks.y = element_blank()
        )
      
      if(num_predictor == 1){
        exp_plot <- exp_plot + theme(strip.text.y = element_blank())
      }else{
        exp_plot <- exp_plot + theme(panel.grid.minor.y = element_blank())
      }
      
      ggarrange(
        factor_grid_plot
        , output_plot
        , exp_plot
        , nrow = 1, ncol = 3
        , widths = c(3, 2, 3)
      )
    }
  }else{
    ## Sort feature columns by exp normalized magnitudes
    sample_column_names <- c("output", levels(nomogram_data$feature))
    sorted_sample_data <- select(sample_data, all_of(sample_column_names))
    
    ## Obtain feature combinations being predicted positive
    pos_pred_features <- list()
    
    if(verbose){
      i <- 0
      pb <- txtProgressBar(min = i, max = ncol(sorted_sample_data) - 1, style = 3)
    }
    
    for(colseq in seq(2, ncol(sorted_sample_data))){
      up_to_colseq <-
        sorted_sample_data |>
        select(all_of(c(1, 2:colseq)))
      
      pos_pred_features[[colseq - 1]] <-
        up_to_colseq |>
        mutate(
          value =
            seq(nrow(up_to_colseq)) |>
            sapply(\(x)  paste0(up_to_colseq[x, -1, drop = TRUE], collapse = "/"))
          , variable = paste0(colnames(up_to_colseq)[-1], collapse = "/")
        )
      
      if(colseq > 2){
        avail_values <- reduce(pos_pred_features[-(colseq - 1)], rbind)$value
        
        if(length(avail_values) > 0){
          pos_pred_features[[colseq - 1]] <-
            pos_pred_features[[colseq - 1]] |>
            filter(
              !str_detect(
                value, paste0(paste0("^", avail_values), collapse = "|")
              )
            )
        }
      }
      
      pos_pred_features[[colseq - 1]] <-
        pos_pred_features[[colseq - 1]] |>
        group_by(variable, value) |>
        summarize(
          min = suppressWarnings(min(output))
          , med = suppressWarnings(median(output))
          , max = suppressWarnings(max(output))
          , .groups = "drop"
        ) |>
        filter(min >= threshold)
      
      if(verbose){
        i <- i + 1
        setTxtProgressBar(pb, i)
      }
    }
    
    if(verbose) close(pb)
    
    pos_pred_features <- reduce(pos_pred_features, rbind)
    
    # Obtain feature combinations being predicted negative
    neg_pred_features <- list()
    
    if(verbose){
      i <- 0
      pb <-
        txtProgressBar(min = i, max = ncol(sorted_sample_data) - 1, style = 3)
    }
    
    for(colseq in rev(seq(2, ncol(sorted_sample_data)))){
      up_to_colseq <-
        sorted_sample_data |>
        select(all_of(c(1, 2:colseq)))
      
      neg_pred_features[[colseq - 1]] <-
        up_to_colseq |>
        mutate(
          value =
            seq(nrow(up_to_colseq)) |>
            sapply(
              \(x) paste0(up_to_colseq[x, -1, drop = TRUE], collapse = "/")
            )
          , variable = paste0(colnames(up_to_colseq)[-1], collapse = "/")
        )
      
      if(colseq < ncol(sorted_sample_data)){
        avail_values <- reduce(neg_pred_features[-(colseq - 1)], rbind)$value
        
        if(length(avail_values) > 0){
          neg_pred_features[[colseq - 1]] <-
            neg_pred_features[[colseq - 1]] |>
            filter(
              !str_detect(
                value, paste0(paste0("^", avail_values), collapse = "|")
              )
            )
        }
      }
      
      neg_pred_features[[colseq - 1]] <-
        neg_pred_features[[colseq - 1]] |>
        group_by(variable, value) |>
        summarize(
          min = suppressWarnings(min(output))
          , med = suppressWarnings(median(output))
          , max = suppressWarnings(max(output))
          , .groups = "drop"
        ) |>
        filter(max < threshold)
      
      if(verbose){
        i <- i + 1
        setTxtProgressBar(pb, i)
      }
    }
    
    if(verbose) close(pb)
    
    neg_pred_features <- reduce(neg_pred_features, rbind)
    
    ## Create nomogram plot data
    nomogram_plot_data <-
      rbind(
        mutate(pos_pred_features, pred = 1)
        , mutate(neg_pred_features, pred = 0)
      ) |>
      mutate(variable_num = str_count(variable, "/") + 1) |>
      mutate(position = seq(n())) |>
      select(pred, variable_num, position, variable, value) |>
      separate_rows(variable, value, sep = "/") |>
      mutate(
        variable = factor(variable, unique(variable))
        , variable =
          paste0('# ', as.numeric(variable), ' ', variable, ' = ____')
        , variable =  factor(variable, rev(unique(variable)))
        , value =
          case_when(
            value == 0 ~ "Negative"
            , value == 1 ~ "Positive"
            , TRUE ~ "Missing"
          )
        , value = ifelse(value == "Missing", NA, value)
        , value = factor(value, c("Negative", "Positive"))
        , pred =
          case_when(
            pred == 0 ~ paste0("Negative prediction")
            ,pred == 1 ~ paste0("Positive prediction")
          )
        , pred = factor(pred)
        , pred = factor(pred, rev(levels(pred)))
      )
    
    nomogram_plot_data |>
      ggplot(aes(position, variable)) +
      geom_tile(aes(fill = factor(value)), color = "white") +
      facet_grid(~pred, scales = "free_x", space = "free_x", switch = "x") +
      scale_x_continuous(
        "Iteration"
        , breaks = seq(1, max(nomogram_plot_data$position), 1)
        , expand = c(0, 0)
        , position = "top"
      ) +
      scale_y_discrete(
        "Maximum-impact rank"
        , expand = c(0, 0)
      ) +
      scale_fill_discrete("Predictor value") +
      theme(
        panel.border = element_rect(linewidth = 0.5, color = "black", fill = NA)
        , panel.spacing = unit(0, "lines")
        , panel.grid = element_blank()
        , axis.title.y = element_text(angle = 90)
        , axis.text.y = element_text(hjust = 0)
        , legend.position = "top"
        , strip.background = element_rect(linewidth = 0.5, color = "black")
      )
  }
}