create_nomogram <- function(sample_features, sample_prob, feature_shap, threshold = 0.5, prob = FALSE, verbose = FALSE){
  # Change feature categories features to binaries
  sample_features0 <- sample_features
  sample_features <- mutate_if(sample_features, is.factor, \(x) as.numeric(x) - 1)
  
  # Combine feature values with predicted probabilities
  sample_data <- cbind(sample_prob, sample_features)
  
  # Combine data and sort feature values sorted by SHAP normalized magnitudes
  nomogram_data <-
    sample_data |>
    mutate(i = seq(nrow(sample_data))) |>
    gather(feature, value, -i, -prob) |>
    left_join(
      feature_shap |>
        mutate(i = seq(nrow(feature_shap))) |>
        gather(feature, shap, -i) 
      , by = join_by(i, feature)
    ) |>
    group_by(feature) |>
    mutate(magnitude = max(shap)) |>
    ungroup() |>
    mutate(
      norm_magnitude =
        (magnitude - min(magnitude)) / (max(magnitude) - min(magnitude))
    ) |>
    mutate(feature = reorder(feature, norm_magnitude)) |>
    arrange(i, feature) |>
    mutate(factor_group = NA)
  
  if(prob){
    nomogram_data_resorted <-
      nomogram_data |>
      left_join(
        nomogram_data |>
          arrange(feature, value) |>
          select(i) |>
          unique() |>
          mutate(i2 = seq(n()))
        , by = join_by(i)
      ) |>
      mutate(i = i2) |>
      select(-i2) |>
      arrange(i, feature, value)
    
    num_predictor <- sum(sapply(sample_features0, is.numeric))
    
    if(num_predictor == 1){
      nomogram_data_factor_spread <-
        nomogram_data_resorted |>
        filter(feature %in% colnames(select_if(sample_features0, is.factor))) |>
        select(i, feature, value) |>
        spread(feature, value)

      nomogram_data_factor_group <-
        nomogram_data_factor_spread |>
        select(-i) |>
        unique() |>
        mutate(factor_group = seq(n()))

      nomogram_data_resorted <-
        nomogram_data_resorted |>
        select(-factor_group) |>
        left_join(
          nomogram_data_factor_spread |>
            left_join(
              nomogram_data_factor_group
              , by = setdiff(colnames(nomogram_data_factor_spread), "i")
            ) |>
            select(i, factor_group)
          , by = join_by(i)
        )
    }
    
    factor_grid_plot <-
      nomogram_data_resorted |>
      filter(feature %in% colnames(select_if(sample_features0, is.factor)))
    
    if(num_predictor == 1){
      factor_grid_plot <-
        factor_grid_plot |>
        mutate(i = 1) |>
        unique()
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
    
    factor_grid_plot <-
      factor_grid_plot +
      scale_x_discrete("Predictor")
    
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
      prob_plot <-
        nomogram_data_resorted |>
        filter(feature %in% colnames(select_if(sample_features0, is.numeric))) |>
        ggplot(aes(prob, value))
    }else{
      prob_plot <-
        nomogram_data_resorted |>
        ggplot(aes(prob, i))
    }
    
    prob_plot <-
      prob_plot +
      geom_vline(xintercept = 0.5, lty = 2) +
      geom_path()
    
    if(num_predictor == 1){
      prob_plot <-
        prob_plot +
        facet_grid(factor_group ~ ., scales = "free_y", space = "free_y")
    }
    
    prob_plot <-
      prob_plot +
      scale_x_continuous("Outcome ->")
    
    if(num_predictor == 1){
      prob_plot <-
        prob_plot +
        ylab(colnames(select_if(sample_features0, is.numeric)))
    }else{
      prob_plot <-
        prob_plot +
        scale_y_continuous(
          breaks = seq(max(nomogram_data$i))
          , limits = c(min(nomogram_data$i) - 0.5, max(nomogram_data$i) + 0.5)
        )
    }
    
    prob_plot <-
      prob_plot +
      theme(
        axis.title.x = element_text(size = 8)
        , axis.text.x = element_text(size = 7)
      )
    
    if(num_predictor == 1){
      prob_plot <-
        prob_plot +
        theme(strip.text.y = element_blank())
    }else{
      prob_plot <-
        prob_plot +
        theme(
          panel.grid.minor.y = element_blank()
          , axis.title.y = element_blank()
          , axis.text.y = element_blank()
          , axis.ticks.y = element_blank()
        )
    }
    
    shap_plot <- nomogram_data_resorted
    
    if(num_predictor == 1){
      shap_plot <-
        shap_plot |>
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
    
    shap_plot <-
      shap_plot |>
      ggplot(aes(shap, i)) +
      geom_vline(xintercept = 0, lty = 2) +
      geom_path(aes(color = feature), na.rm = TRUE)
    
    if(num_predictor == 1){
      shap_plot <-
        shap_plot +
        facet_grid(factor_group ~ .)
    }
    
    shap_plot <-
      shap_plot +
      scale_x_continuous("Impact on fatigue ->")
    
    if(num_predictor != 1){
      shap_plot <-
        shap_plot +
        scale_y_continuous(
          breaks = seq(max(nomogram_data$i))
          , limits = c(min(nomogram_data$i) - 0.5, max(nomogram_data$i) + 0.5)
        )
    }
    
    shap_plot <-
      shap_plot +
      scale_color_discrete("Predictor") +
      theme(
        axis.title.x = element_text(size = 8)
        , axis.text.x = element_text(size = 7)
        , axis.title.y = element_blank()
        , axis.text.y = element_blank()
        , axis.ticks.y = element_blank()
      )
    
    if(num_predictor == 1){
      shap_plot <-
        shap_plot +
        theme(strip.text.y = element_blank())
    }else{
      shap_plot <-
        shap_plot +
        theme(panel.grid.minor.y = element_blank())
    }
    
    ggarrange(
      factor_grid_plot
      , prob_plot
      , shap_plot
      , nrow = 1, ncol = 3
      , widths = c(3, 2, 3)
    )
  }else{
    # Sort feature columns by SHAP normalized magnitudes
    sample_column_names <- c("prob", levels(nomogram_data$feature))
    sorted_sample_data <- select(sample_data, all_of(sample_column_names))
    
    # Obtain feature combinations being predicted positive
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
              !str_detect(value, paste0(paste0("^", avail_values), collapse = "|"))
            )
        }
      }
      
      pos_pred_features[[colseq - 1]] <-
        pos_pred_features[[colseq - 1]] |>
        group_by(variable, value) |>
        summarize(
          min = suppressWarnings(min(prob))
          , med = suppressWarnings(median(prob))
          , max = suppressWarnings(max(prob))
          , .groups = "drop"
        ) %>%
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
      pb <- txtProgressBar(min = i, max = ncol(sorted_sample_data) - 1, style = 3)
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
            sapply(\(x) paste0(up_to_colseq[x, -1, drop = TRUE], collapse = "/"))
          , variable = paste0(colnames(up_to_colseq)[-1], collapse = "/")
        )
      
      if(colseq < ncol(sorted_sample_data)){
        avail_values <- reduce(neg_pred_features[-(colseq - 1)], rbind)$value
        
        if(length(avail_values) > 0){
          neg_pred_features[[colseq - 1]] <-
            neg_pred_features[[colseq - 1]] |>
            filter(
              !str_detect(value, paste0(paste0("^", avail_values), collapse = "|"))
            )
        }
      }
      
      neg_pred_features[[colseq - 1]] <-
        neg_pred_features[[colseq - 1]] |>
        group_by(variable, value) |>
        summarize(
          min = suppressWarnings(min(prob))
          , med = suppressWarnings(median(prob))
          , max = suppressWarnings(max(prob))
          , .groups = "drop"
        ) %>%
        filter(max < threshold)
      
      if(verbose){
        i <- i + 1
        setTxtProgressBar(pb, i)
      }
    }
    
    if(verbose) close(pb)
    
    neg_pred_features <- reduce(neg_pred_features, rbind)
    
    # Create nomogram plot data
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
        , variable = paste0('# ', as.numeric(variable), ' ', variable, ' = ____')
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