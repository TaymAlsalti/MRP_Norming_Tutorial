# =======================
# HELPER: Build final 3 aggregated plots
# =======================
norm_plots <- function(means_ns_sds_and_ses, labels, palette, ps_table) {
  
  # We do the same "expand" logic used in the main function’s percentile code
  maxLabelLen <- if (is.null(labels)) 1 else max(nchar(labels))
  myExpandRight <- maxLabelLen/1.7
  
  # 1) Factor levels => color scale (for aggregated means, etc.)
  my_levels <- levels(means_ns_sds_and_ses$source)
  if (length(palette) != length(my_levels)) {
    stop("Length of 'palette' must match the final number of factor levels.\n",
         "We have ", length(my_levels), " levels: ",
         paste(my_levels, collapse = ", "), "\n",
         "But you supplied ", length(palette), " color(s).")
  }
  color_scale <- ggplot2::scale_colour_manual(
    breaks = my_levels,
    values = palette
  )
  
  # 2) Common theme
  custom_theme <- ggplot2::theme(
    axis.text.x       = ggplot2::element_text(angle=45, hjust=1),
    panel.grid.major.y= ggplot2::element_blank(),
    panel.grid.minor.y= ggplot2::element_blank(),
    panel.grid.minor.x= ggplot2::element_blank(),
    panel.grid.major.x= ggplot2::element_line(linetype="dashed", linewidth=0.3),
    legend.position   = "none"
  )
  
  # We'll label lines at the max age in ps_table
  max_age_val <- max(ps_table$age, na.rm=TRUE)
  
  # 3) Means plot
  means_plot <- means_ns_sds_and_ses %>%
    ggplot2::ggplot(ggplot2::aes(x=age, y=mean, group=source, colour=source)) +
    ggplot2::scale_x_continuous(
      breaks = seq(min(ps_table$age), max(ps_table$age), by=2),
      expand = ggplot2::expansion(add=c(1,myExpandRight))
    ) +
    ggplot2::geom_line(linewidth=1) +
    ggplot2::geom_pointrange(
      shape=18, fatten=3, linewidth=1,
      ggplot2::aes(ymin=mean - 1.96*seOfmean, ymax=mean + 1.96*seOfmean),
      position=ggplot2::position_dodge(width=0.3)
    ) +
    ggrepel::geom_text_repel(
      data = dplyr::filter(means_ns_sds_and_ses, age==max_age_val),
      ggplot2::aes(label=source),
      family  = "Times",
      seed    = 810,
      nudge_x = 1,
      hjust   = 0,
      point.padding = 1,
      direction    = "y"
    ) +
    custom_theme + color_scale +
    ggplot2::labs(x="Age", y="Mean")
  
  # 4) SDs plot
  sds_plot <- means_ns_sds_and_ses %>%
    ggplot2::ggplot(ggplot2::aes(x=age, y=sd, group=source, colour=source)) +
    ggplot2::scale_x_continuous(
      breaks = seq(min(ps_table$age), max(ps_table$age), by=2),
      expand = ggplot2::expansion(add=c(1,myExpandRight))
    ) +
    ggplot2::geom_line(linewidth=1) +
    ggplot2::geom_pointrange(
      shape=18, fatten=3, linewidth=1,
      ggplot2::aes(ymin=sd - 1.96*seOfsd, ymax=sd + 1.96*seOfsd),
      position=ggplot2::position_dodge(width=0.3)
    ) +
    ggrepel::geom_text_repel(
      data = dplyr::filter(means_ns_sds_and_ses, age==max_age_val),
      ggplot2::aes(label=source),
      family  = "Times",
      seed    = 810,
      nudge_x = 1,
      hjust   = 0,
      point.padding = 1,
      direction    = "y"
    ) +
    custom_theme + color_scale +
    ggplot2::labs(x="Age", y="SD")
  
  # 5) SE of Mean plot
  ses_plot <- means_ns_sds_and_ses %>%
    ggplot2::ggplot(ggplot2::aes(x=age, y=seOfmean, group=source, colour=source)) +
    ggplot2::scale_x_continuous(
      breaks = seq(min(ps_table$age), max(ps_table$age), by=2),
      expand = ggplot2::expansion(add=c(1,myExpandRight))
    ) +
    ggplot2::geom_line(linewidth=1) +
    ggplot2::geom_point(shape=18, size=3) +
    ggrepel::geom_text_repel(
      data = dplyr::filter(means_ns_sds_and_ses, age==max_age_val),
      ggplot2::aes(label=source),
      family  = "Times",
      seed    = 810,
      nudge_x = 1,
      hjust   = 0,
      point.padding = 1
    ) +
    custom_theme + color_scale +
    ggplot2::labs(x="Age", y="SE of Mean")
  
  list(means_plot = means_plot, SDs_plot = sds_plot, SEs_plot = ses_plot)
}



age_norm_comparisons <- function(...,
                                 ps_table       = census,  
                                 ps_variables   = NULL,  # Changed default to NULL
                                 re_formula     = NULL,
                                 sim_size       = 100000,
                                 RP             = NULL,        
                                 labels         = NULL,
                                 palette        = c("#BC3C29FF", "#0072B5FF"),
                                 output_file    = "../02_data/results.rds",
                                 prediction_transform = NULL  # function or list of functions or NULL
) {
  
  #--------------------------------------------------
  # 1) If output_file exists => load => REBUILD everything 
  #    (including percentile) from stored data frames
  #--------------------------------------------------
  if (file.exists(output_file)) {
    message("File '", output_file, "' already exists. Loading from RDS...")
    existing_data <- readRDS(output_file)
    
    means_ns_sds_and_ses <- existing_data$means_ns_sds_and_ses
    overall_estimates    <- existing_data$overall_estimates
    percentile_data      <- existing_data$percentile_data
    
    plots <- norm_plots(means_ns_sds_and_ses, labels, palette, ps_table)
    
    percentile_plot <- NULL
    if (!is.null(percentile_data)) {
      approach_set <- levels(percentile_data$Source)
      if ("Raw" %in% approach_set) {
        color_values <- palette
      } else {
        color_values <- palette[-1]
      }
      
      baseMaxLabelLen <- if (is.null(labels)) 1 else max(nchar(labels))
      maxLabelLen2     <- max(nchar(percentile_data$line_label), na.rm=TRUE)
      myExpandRight2   <- max(baseMaxLabelLen, maxLabelLen2, 1)/1.7
      
      max_age_val <- max(percentile_data$age, na.rm=TRUE)
      percentile_plot <- percentile_data %>%
        ggplot2::ggplot(
          ggplot2::aes(x=age, y=Raw_score,
                       color=Source,
                       group=interaction(Source, percentile_label))
        ) +
        ggplot2::geom_step(linewidth=1, linetype=1) +
        ggplot2::scale_x_continuous(
          breaks = seq(min(ps_table$age), max(ps_table$age), by=2),
          expand = ggplot2::expansion(add=c(1,myExpandRight2))
        ) +
        ggplot2::scale_color_manual(values=color_values, name="") +
        ggplot2::geom_text(
          data = dplyr::filter(percentile_data, age==max_age_val, show_text, !is.na(line_label)),
          ggplot2::aes(label=line_label),
          nudge_x=1,
          hjust=0,
          family="Times"
        ) +
        ggplot2::labs(x="Age", y="Test score", color="") +
        ggplot2::theme(
          axis.text.x       = ggplot2::element_text(angle=45, hjust=1),
          panel.grid.major.y= ggplot2::element_blank(),
          panel.grid.minor.y= ggplot2::element_blank(),
          panel.grid.minor.x= ggplot2::element_blank(),
          panel.grid.major.x= ggplot2::element_line(linetype="dashed", linewidth=0.3),
          legend.position   = "top"
        )
    }
    
    final_list <- list(
      means_ns_sds_and_ses = means_ns_sds_and_ses,
      overall_estimates    = overall_estimates,
      means_plot           = plots$means_plot,
      SDs_plot             = plots$SDs_plot,
      SEs_plot             = plots$SEs_plot,
      percentile_plot      = percentile_plot
    )
    
    rm(existing_data, plots, percentile_data)
    gc(verbose=FALSE)
    return(final_list)
  }
  
  # If we get here => new simulation
  #--------------------------------------------------
  # 2) If RP is NULL => default "census"
  #--------------------------------------------------
  if (is.null(RP)) {
    RP <- "census"
  }
  if (is.character(RP) && length(RP)==1) {
    RP <- c(RP)
  }
  
  #--------------------------------------------------
  # 3) Gather brms models
  #--------------------------------------------------
  brms_models    <- list(...)
  raw_call_names <- sapply(substitute(list(...))[-1], deparse)
  if (!all(sapply(brms_models, inherits, "brmsfit"))) {
    stop("All '...' must be brmsfit models.")
  }
  nModels <- length(brms_models)
  
  # Handle ps_variables
  if (is.null(ps_variables)) {
    # If NULL, use all predictors from each model
    ps_variables <- lapply(brms_models, function(model) {
      all.vars(model$formula$formula)[-1]  # Exclude response variable
    })
  } else if (!is.list(ps_variables)) {
    # If not a list, replicate for all models
    ps_variables <- replicate(nModels, ps_variables, simplify = FALSE)
  } else if (length(ps_variables) != nModels) {
    stop("If ps_variables is a list, its length must match number of brms models.")
  }
  
  # Handle prediction_transform
  if (!is.null(prediction_transform)) {
    if (is.function(prediction_transform)) {
      prediction_transform <- replicate(nModels, prediction_transform, simplify=FALSE)
    } else if (is.list(prediction_transform)) {
      if (length(prediction_transform)!=nModels) {
        stop("'prediction_transform' must match # of brms models if it's a list.")
      }
    } else {
      stop("'prediction_transform' must be NULL or a function or list of functions.")
    }
  } else {
    prediction_transform <- replicate(nModels, NULL, simplify=FALSE)
  }
  
  #--------------------------------------------------
  # 4) Summarize the "raw" lines from the FIRST brms model
  #--------------------------------------------------
  first_mod_data <- brms_models[[1]]$data
  out_name <- all.vars(brms_models[[1]]$formula$formula)[1]
  if (is.na(out_name)) {
    stop("Could not detect outcome from first brms model (multi-param?).")
  }
  first_mod_data[[ out_name ]] <- as.numeric(as.character(first_mod_data[[ out_name ]]))
  
  means_sds_and_ses_raw <- first_mod_data %>%
    dplyr::mutate(age = floor(age)) %>%
    dplyr::group_by(age) %>%
    dplyr::summarise(
      Raw_n    = dplyr::n(),
      Raw_mean = mean(.data[[ out_name ]], na.rm=TRUE),
      Raw_sd   = sd(.data[[ out_name ]], na.rm=TRUE),
      Raw_seOfmean = Raw_sd / sqrt(Raw_n),
      .groups="drop"
    )
  
  row_level_draws <- list()
  row_level_draws[["raw"]] <- list()
  row_level_draws[["raw"]][[ raw_call_names[1] ]] <- first_mod_data %>%
    dplyr::mutate(
      age         = floor(age),
      .draw       = 1,
      .prediction = .data[[ out_name ]]
    )
  
  age_level_list <- list()
  
  #--------------------------------------------------
  # 5) Loop over each approach in RP
  #--------------------------------------------------
  for (this_approach in RP) {
    prefix <- if (this_approach=="census") "RPP_" else "RP_"
    
    approach_age_wide_list <- list()
    row_level_draws[[ this_approach ]] <- list()
    
    for (i in seq_along(brms_models)) {
      model      <- brms_models[[i]]
      model_name <- raw_call_names[i]
      short_name <- sub("^brm_", "", model_name)
      full_name  <- paste0(prefix,"brm_",short_name)
      
      # Get the ps_variables for this model
      this_ps_vars <- ps_variables[[i]]
      
      # Create simulation data with the appropriate variables
      if (this_approach=="census") {
        sim_data <- ps_table %>%
          dplyr::filter(census_n != 0) %>%
          dplyr::ungroup() %>%
          dplyr::sample_n(
            size   = sim_size,
            weight = census_n,
            replace=TRUE
          ) 
      } else if (this_approach=="norming_sample") {
        sim_data <- first_mod_data %>%
          dplyr::select(dplyr::all_of(this_ps_vars)) %>%
          dplyr::mutate(age = floor(age)) %>%
          dplyr::filter(between(age, min(ps_table$age), max(ps_table$age))) %>% 
          dplyr::group_by(dplyr::across(dplyr::everything())) %>%
          dplyr::summarise(Raw_n = dplyr::n(), .groups="drop") %>%
          dplyr::sample_n(
            size   = sim_size,
            weight = Raw_n,
            replace=TRUE
          ) 
      } else {
        stop("RP must be 'census' or 'norming_sample'.")
      } 
      
      sim_data_with_draws <- sim_data %>% 
        mutate(pid = row_number() + 100000) %>%  
        select(pid, everything()) %>%
        tidybayes::add_predicted_draws(
          model,
          ndraws           = 1000,
          seed             = 810,
          re_formula       = re_formula,
          allow_new_levels = TRUE
        )
      
      fun_pred <- prediction_transform[[ i ]]
      if (!is.null(fun_pred)) {
        sim_data_with_draws$.prediction <- fun_pred(sim_data_with_draws$.prediction)
      }
      
      summary_df <- sim_data_with_draws %>%
        dplyr::group_by(age, .draw) %>%
        dplyr::summarise(
          mean_prediction = mean(.prediction),
          sd_prediction   = sd(.prediction),
          .groups         = "drop"
        ) %>%
        dplyr::group_by(age) %>%
        dplyr::summarise(
          !!paste0(full_name,"_mean")     := mean(mean_prediction),
          !!paste0(full_name,"_seOfmean") := stats::sd(mean_prediction),
          !!paste0(full_name,"_sd")       := sqrt(mean(sd_prediction^2)),
          !!paste0(full_name,"_seOfsd")   := stats::sd(sd_prediction),
          .groups="drop"
        )
      
      approach_age_wide_list[[ model_name ]] <- summary_df
      row_level_draws[[ this_approach ]][[ model_name ]] <- sim_data_with_draws
    }
    
    approach_age_wide <- purrr::reduce(approach_age_wide_list, dplyr::left_join, by="age")
    age_level_list[[ this_approach ]] <- approach_age_wide
  }
  
  #--------------------------------------------------
  # 6) Combine age-level results
  #--------------------------------------------------
  if (length(age_level_list)==1) {
    combined_results <- age_level_list[[1]]
  } else if (length(age_level_list)>1) {
    combined_results <- purrr::reduce(age_level_list, dplyr::left_join, by="age")
  } else {
    combined_results <- means_sds_and_ses_raw
  }
  
  combined_results <- combined_results %>%
    dplyr::left_join(means_sds_and_ses_raw, by="age")
  
  #--------------------------------------------------
  # 7) Pivot to long
  #--------------------------------------------------
  means_ns_sds_and_ses <- combined_results %>%
    tidyr::pivot_longer(
      -age,
      names_to     = c("source",".value"),
      names_pattern= "(.*)_(.*)"
    )
  
  final_levels <- "Raw"
  for (i in seq_along(brms_models)) {
    model_name <- raw_call_names[i]
    short_name <- sub("^brm_","",model_name)
    if ("census" %in% RP) {
      final_levels <- c(final_levels, paste0("RPP_brm_", short_name))
    }
    if ("norming_sample" %in% RP) {
      final_levels <- c(final_levels, paste0("RP_brm_", short_name))
    }
  }
  existing_sources <- unique(means_ns_sds_and_ses$source)
  level_order     <- final_levels[final_levels %in% existing_sources]
  leftover        <- setdiff(existing_sources, level_order)
  if (length(leftover)>0) {
    level_order <- c(level_order, leftover)
  }
  means_ns_sds_and_ses <- means_ns_sds_and_ses %>%
    dplyr::mutate(source = factor(source, levels=level_order))
  
  if (!is.null(labels)) {
    if (length(labels) != length(level_order)) {
      stop("Length of 'labels' must match final # of sources.\n",
           "We have ", length(level_order)," sources in order: ",
           paste(level_order, collapse=", "),"\n",
           "But you supplied ", length(labels), " labels.")
    }
    means_ns_sds_and_ses$source <- forcats::fct_relabel(
      means_ns_sds_and_ses$source,
      function(x) labels[match(x, level_order)]
    )
  }
  
  #--------------------------------------------------
  # 8) overall_estimates
  #--------------------------------------------------
  overall_estimates <- list()
  for (appr in names(row_level_draws)) {
    prefix <- if (appr=="census") "RPP_" else if (appr=="norming_sample") "RP_" else "Raw_"
    
    for (i in seq_along(brms_models)) {
      model_name <- raw_call_names[i]
      short_name <- sub("^brm_","",model_name)
      full_label <- if (appr=="raw") {
        "Raw"
      } else {
        paste0(prefix, "brm_", short_name)
      }
      
      df_sim <- row_level_draws[[ appr ]][[ model_name ]]
      if (!is.data.frame(df_sim)) next
      
      sum_df <- df_sim %>%
        dplyr::group_by(.draw) %>%
        dplyr::summarise(
          mean_prediction=mean(.prediction),
          sd_prediction  =sd(.prediction),
          .groups="drop"
        ) %>%
        dplyr::summarise(
          Mean       = mean(mean_prediction),
          SE_of_Mean = stats::sd(mean_prediction),
          SD         = sqrt(mean(sd_prediction^2)),
          SE_of_SD   = stats::sd(sd_prediction),
          .groups    = "drop"
        ) %>%
        dplyr::mutate(Model=full_label)
      
      overall_estimates[[ length(overall_estimates)+1 ]] <- sum_df
    }
  }
  overall_estimates <- dplyr::bind_rows(overall_estimates)
  
  #--------------------------------------------------
  # Build percentile_data from row_level_draws 
  #--------------------------------------------------
  approach_set <- setdiff(names(row_level_draws), "raw")
  target_p <- c(0.01,0.05,0.5,0.95,0.99)
  df_percentile_curves <- list()
  
  for (appr_name in approach_set) {
    for (i in seq_along(brms_models)) {
      modn    <- raw_call_names[i]
      shortn  <- sub("^brm_", "", modn)
      
      prefix_line <- if (appr_name=="census") "RPP_" else "RP_"
      df_sim  <- row_level_draws[[appr_name]][[ modn ]]
      if (!is.data.frame(df_sim)) next
      if (!is.numeric(df_sim$.prediction)) next
      
      source_string <- paste0(prefix_line,"brm_", shortn)
      
      df_percent <- df_sim %>%
        dplyr::group_by(age) %>%
        dplyr::arrange(.prediction, .by_group=TRUE) %>%
        dplyr::mutate(
          N = dplyr::n(),
          percentile = (dplyr::row_number() - 0.5)/N
        ) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(age) %>%
        dplyr::summarise(
          pvals = list(target_p),
          scores= list(stats::approx(x=percentile, y=.prediction,
                                     xout=target_p, ties="ordered")$y),
          .groups="drop"
        ) %>%
        tidyr::unnest(cols=c(pvals,scores)) %>%
        dplyr::rename(percentile_value=pvals, Raw_score=scores) %>%
        dplyr::mutate(Source=source_string)
      
      df_percentile_curves[[ length(df_percentile_curves)+1 ]] <- df_percent
    }
  }
  
  df_percentile_final <- dplyr::bind_rows(df_percentile_curves) %>%
    dplyr::mutate(
      percentile_label = dplyr::case_when(
        abs(percentile_value-0.01)<1e-9 ~ "1st percentile",
        abs(percentile_value-0.05)<1e-9 ~ "5th percentile",
        abs(percentile_value-0.5) <1e-9 ~ "50th percentile",
        abs(percentile_value-0.95)<1e-9 ~ "95th percentile",
        abs(percentile_value-0.99)<1e-9 ~ "99th percentile",
        TRUE ~ paste0(round(100*percentile_value,1), "th percentile")
      )
    )
  
  df_percentile_final$Source <- factor(df_percentile_final$Source, levels=level_order[level_order != "Raw"])
  
  if (!is.null(labels) && length(labels) == length(level_order)) {
    adjusted_labels <- labels[-1]
    level_order_no_raw <- level_order[level_order != "Raw"]
    
    if (length(adjusted_labels) != length(level_order_no_raw)) {
      stop("Number of labels doesn't match number of sources for percentiles.")
    }
    
    df_percentile_final$Source <- forcats::fct_relabel(
      df_percentile_final$Source,
      function(x) adjusted_labels[match(x, level_order_no_raw)]
    )
  }
  
  methodLevels <- levels(df_percentile_final$Source)
  df_percentile_final <- df_percentile_final %>%
    dplyr::mutate(
      show_text = Source == methodLevels[1],
      line_label = dplyr::if_else(show_text, percentile_label, NA_character_)
    )
  
  percentile_data <- df_percentile_final
  
  # Save result
  final_data <- list(
    means_ns_sds_and_ses = means_ns_sds_and_ses,
    overall_estimates    = overall_estimates,
    percentile_data      = percentile_data
  )
  saveRDS(final_data, file=output_file, compress="xz")
  
  # Plots
  plots <- norm_plots(means_ns_sds_and_ses, labels, palette, ps_table)
  
  # Build percentile plot
  approach_set <- levels(percentile_data$Source)
  color_values <- palette[-1]
  
  baseMaxLabelLen <- if (is.null(labels)) 1 else max(nchar(labels))
  maxLabelLen2    <- max(nchar(percentile_data$line_label), na.rm=TRUE)
  myExpandRight2  <- max(baseMaxLabelLen, maxLabelLen2, 1)/1.7
  max_age_val     <- max(percentile_data$age, na.rm=TRUE)
  
  percentile_plot <- percentile_data %>%
    ggplot2::ggplot(
      ggplot2::aes(x=age, y=Raw_score,
                   color=Source,
                   group=interaction(Source, percentile_label))
    ) +
    ggplot2::geom_step(linewidth=1, linetype=1) +
    ggplot2::scale_x_continuous(
      breaks = seq(min(ps_table$age), max(ps_table$age), by=2),
      expand = ggplot2::expansion(add=c(1,myExpandRight2))
    ) +
    ggplot2::scale_color_manual(values=color_values, name="") +
    ggplot2::geom_text(
      data = dplyr::filter(percentile_data, age==max_age_val, show_text, !is.na(line_label)),
      ggplot2::aes(label=line_label),
      nudge_x=1,
      hjust=0,
      family="Times"
    ) +
    ggplot2::labs(x="Age", y="Test score", color="") +
    ggplot2::theme(
      axis.text.x       = ggplot2::element_text(angle=45, hjust=1),
      panel.grid.major.y= ggplot2::element_blank(),
      panel.grid.minor.y= ggplot2::element_blank(),
      panel.grid.minor.x= ggplot2::element_blank(),
      panel.grid.major.x= ggplot2::element_line(linetype="dashed", linewidth=0.3),
      legend.position   = "top"
    )
  
  list(
    means_ns_sds_and_ses = means_ns_sds_and_ses,
    overall_estimates    = overall_estimates,
    means_plot           = plots$means_plot,
    SDs_plot             = plots$SDs_plot,
    SEs_plot             = plots$SEs_plot,
    percentile_plot      = percentile_plot
  )
}