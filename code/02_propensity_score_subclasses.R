# Load preamble settings and functions
source(file.path("code", "00_preamble.R"), echo = TRUE)

# Set multi-core processing option and create a parallel cluster
multi_core <- TRUE
cl <- parallel::makePSOCKcluster(ifelse(multi_core, 20, 1))  # Create cluster with 20 cores if multi_core is TRUE, otherwise use 1 core
doParallel::registerDoParallel(cl)  # Register the parallel backend for foreach

# Perform parallel processing on rows of do_tab
ps_subclass_outl <- foreach::foreach(
  j = 1:nrow(do_tab),  # Loop through each row of the do_tab dataset
  .verbose = TRUE,  # Print progress information during execution
  .packages = c("tidyverse", "dplyr", "magrittr", "qqplotr", "ggplot2", "randomForest"),  # Load necessary packages
  .errorhandling = "pass"  # Continue execution even if an error occurs
) %dopar% {
  
  # Try-catch block to handle errors gracefully
  try_output <- try({
    
    # Retrieve current project information and parameters from do_tab
    proj_id <- do_tab$proj_id[j]  # Current project ID
    caliper_val <- do_tab$caliper_val[j]  # Caliper value for matching
    dist_use <- do_tab$dist_use[j]  # Distance measure for matching
    match_w_replacement <- do_tab$match_w_replacement[j] # repl
  
    file_name <- paste0("design_proj_", proj_id, "_caliper_", caliper_val, "_dist_", dist_use, "_replace_", match_w_replacement, ".RDS")

    # Check if any output files do not exist
    # ls -l . | egrep -c '^-'
    if (!all(file.exists(file.path(d_matched_dir, file_name)), 
            file.exists(file.path(prop_score_dir, file_name)), 
            file.exists(file.path(match_out_dir, file_name)))) {
      cat(sprintf("Not all files exist for project %s. Proceeding with further examination.\n", proj_id))
      # Your code for further examination
    } else {
      cat(sprintf("All files exist for project %s. Skipping.\n", proj_id))
      return(NULL)
    }
    
    # Load the pre-processed quality-controlled data for the current project
    d_qc <- readRDS(file = file.path(dir_qc_data, paste0("qc_data_", proj_id, ".RDS")))
    # 'reinforce' treat as a binary variable
    d_qc$treat <- as.integer(d_qc$treat)
    
    #########################################
    # MATCHING PROCESS
    #########################################
    # Set up covariates and calipers for matching
    cov_terms <- attr(x = terms(ps_score_form), which = "term.labels")  # Get covariate terms from the propensity score formula
    all_terms <- c("treat", cov_terms)  # Combine treatment indicator and covariates
    calipers <- rep(caliper_val, length(cov_terms))  # Set calipers for each covariate
    names(calipers) <- cov_terms  # Assign names to the calipers
    
    # Set seed for reproducibility
    set.seed(1234)
    # Perform matching using nearest neighbor with replacement and caliper restrictions
    match_out_dist <- MatchIt::matchit(formula = ps_score_form,
                                      data = d_qc,
                                      method = "nearest", 
                                      distance = dist_use,
                                      replace = match_w_replacement,  # Matching with or without replacement
                                      caliper = calipers,
                                      estimand = "ATT")  # Estimate ATT (average treatment effect on treated)
    
    # Process matched data and record the number of times each control unit was matched
    match_matrix <- match_out_dist$match.matrix  # Retrieve match matrix
    duplicated_controls_by_row <- table(match_matrix)  # Count how often each control unit was used
    control_match_tab <- tibble(row = as.numeric(names(duplicated_controls_by_row)), 
                                times_matched = duplicated_controls_by_row)  # Create table for control matches
    control_match_tab %<>% mutate(gid = d_qc$gid[row])  # Add control unit IDs (gid)
    
    # Create a matched dataset with appropriate weights and duplicated controls
    d_matched <- d_qc %>%
      mutate(weight = match_out_dist$weights) %>%
      mutate(rounded_weight = round(weight)) %>%
      mutate(matched = match_out_dist$weights != 0) %>%
      filter(matched) %>%
      left_join(control_match_tab, by = "gid") %>%
      mutate(times_matched = ifelse(treat == 1, 1, times_matched)) %>%
      uncount(times_matched)  # Duplicate control units according to the number of matches
    
    # Scale covariates for matched data
    for (var_curr in cov_terms) {
      d_matched[[var_curr]] <- c(scale(d_matched[[var_curr]]))  # Standardize each covariate
    }
    
    # Display summary of matching weights
    table(match_out_dist$weights)
    table(d_matched$rounded_weight, d_matched$treat)
    
    #########################################
    # PROPENSITY SCORE ESTIMATION
    #########################################
    # Fit full and null models to estimate the propensity scores
    full_model <- glm(formula = treat ~ .*., data = d_matched[, all_terms], family = "binomial")  # Full model with interactions
    null_model <- glm(formula = treat ~ ., data = d_matched[, all_terms], family = "binomial")  # Null model without interactions
    
    # Set seed for reproducibility
    set.seed(1234)
    # Perform stepwise model selection based on AIC to get the final propensity score model
    step_model <- MASS::stepAIC(object = null_model,
                                direction = "forward",
                                scope = list(lower = null_model, upper = full_model), 
                                trace = 0,
                                k = log(nrow(d_matched)))  # Select the final model using forward selection
    
    # Estimate the propensity scores using the selected model
    ps_score_out <- MatchIt::matchit(formula = step_model$formula,
                                     data = d_matched,
                                     method = NULL, 
                                     distance = "glm")  # Estimate propensity scores using GLM
    raw_ps_score <- ps_score_out$model$fitted.values  # Extract raw propensity scores
    d_matched %<>%
      mutate(raw_ps_score = raw_ps_score) %>%
      mutate(ps_score = log(raw_ps_score / (1 - raw_ps_score)))  # Convert to log-odds (logit) scale
    
    #########################################
    # CREATE SUBCLASSES BASED ON PROPENSITY SCORES
    #########################################
    # Set thresholds and constraints for subclass creation
    t_stat_threshold <- 2  # Threshold for t-statistic
    min_treat_units <- 5  # Minimum number of treated units in each block
    min_control_units <- 5  # Minimum number of control units in each block
    min_total_units <- length(cov_terms) + 2  # Minimum total number of units per block
    
    # Function to recursively split blocks based on propensity scores
    split_blocks <- function(data, threshold, min_treat, min_control, min_total, block_id = 1) {
      
      if (!"ps_score" %in% names(data)) {
        stop("Need to have 'ps_score' field in data")
      }
      
      # Base case: Assign block ID if the data has fewer than the minimum units
      if (nrow(data) < min_total) {
        data$block_id <- block_id
        return(data)
      }
      
      # Calculate the t-statistic for the current block and split based on constraints
      t_stat <- abs(t.test(data$ps_score[data$treat == 1], data$ps_score[data$treat == 0])$statistic)
      num_treat_units <- sum(data$treat == 1)
      num_control_units <- sum(data$treat == 0)
      
      if (t_stat <= threshold || num_treat_units < min_treat || num_control_units < min_control) {
        data$block_id <- block_id
        return(data)
      }
      
      # Recursively split the data based on median propensity score
      median_ps <- median(data$ps_score)
      lower_block <- data %>% filter(ps_score <= median_ps)
      upper_block <- data %>% filter(ps_score > median_ps)
      
      # Check if the new blocks meet constraints, otherwise assign block ID
      if (nrow(lower_block) < min_total || nrow(upper_block) < min_total ||
          sum(lower_block$treat == 1) < min_treat || sum(lower_block$treat == 0) < min_control ||
          sum(upper_block$treat == 1) < min_treat || sum(upper_block$treat == 0) < min_control) {
        data$block_id <- block_id
        return(data)
      }
      
      # Recursively split and combine results
      lower_splits <- split_blocks(lower_block, threshold, min_treat, min_control, min_total, block_id * 2)
      upper_splits <- split_blocks(upper_block, threshold, min_treat, min_control, min_total, block_id * 2 + 1)
      
      return(bind_rows(lower_splits, upper_splits))
    }
    
    # Perform the recursive block splitting and assign subclass IDs
    d_matched <- split_blocks(data = d_matched, 
                              threshold = t_stat_threshold, 
                              min_treat = min_treat_units, 
                              min_control = min_control_units, 
                              min_total = min_total_units)
    d_matched %<>%
      mutate(subclass = block_id)
    subclass_unique <- unique(d_matched$subclass)
    
    #########################################
    # CONDITIONAL INDEPENDENCE CHECK
    #########################################
    # Compute z-statistics for balance checks between treatment and covariates within subclasses
    z_stats <- tibble()
    for (subclass_curr in subclass_unique) {
      for (term_curr in cov_terms) {
        d_curr <- d_matched %>%
          filter(subclass == subclass_curr)
        treat_x <- d_curr %>%
          filter(treat == 1) %>%
          pull(term_curr)
        control_x <- d_curr %>%
          filter(treat == 0) %>%
          pull(term_curr)
        s2_stratum <- (sum((treat_x - mean(treat_x))^2) + sum((control_x - mean(control_x))^2)) / (nrow(d_curr) - 2)
        num <- mean(treat_x) - mean(control_x)
        denom <- sqrt(s2_stratum * (1 / length(treat_x) + 1 / length(control_x)))
        if (num == 0 & denom == 0) {
          z_stat <- 0
        } else {
          z_stat <- num / denom
        }
        z_stats %<>% bind_rows(tibble(subclass = subclass_curr,
                                      term = term_curr,
                                      z = z_stat))
      }
    }
    
    # Ensure no NA values in z-statistics, otherwise stop execution
    if (any(is.na(z_stats$z))) {
      stop()
    }
    
    # Loop to iteratively remove subclasses based on QQ plot results and check for balance
    subclass_remove_list <- list()
    for (alpha_curr in qqplot_alpha_vec) {
      subclass_remove <- c()
      data_ok <- FALSE
      while (!data_ok) {
        z_stats_remaining <- z_stats[!z_stats$subclass %in% subclass_remove, ]
        sample_data <- z_stats_remaining$z
        plt <- ggplot(data.frame(sample_data), aes(sample = sample_data)) +
          stat_qq_band(dparams = list(mean = 0, sd = 1), 
                       conf = 1 - alpha_curr, 
                       bandType = "ell", 
                       identity = TRUE) +
          geom_abline(slope = 1, intercept = 0) +
          qqplotr::stat_qq_line(dparams = list(mean = 0, sd = 1), 
                                color = "red", 
                                identity = TRUE) +
          labs(title = "Q-Q Plot with Confidence Bands", x = "Theoretical Quantiles", y = "Sample Quantiles") +
          theme_minimal()
        pg <- ggplot_build(plt)
        sample_data_sorted <- sort(sample_data)
        lower_tail_heavy <- sample_data_sorted < pg$data[[1]]$ymin & pg$data[[1]]$x < 0
        upper_tail_heavy <- sample_data_sorted > pg$data[[1]]$ymax & pg$data[[1]]$x > 0
        outside_band <- lower_tail_heavy | upper_tail_heavy
        plt <- plt + 
          stat_qq(dparams = list(mean = 0, sd = 1), col = ifelse(outside_band, 2, 1))
        if (!any(outside_band)) {
          data_ok <- TRUE
        } else {
          subclass_remove <- unique(c(subclass_remove, unique(z_stats_remaining$subclass[outside_band])))
        }
      }
      print(plt)
      subclass_remove_list[[as.character(alpha_curr)]] <- subclass_remove
    }
    
    # Store outputs including matched data, z-stats, and QQ plot
    d_matched_out <- list(d_matched = d_matched,
                          z_stats = z_stats,
                          subclass_remove_list = subclass_remove_list,
                          qq_plot = plt)
    
    # Save outputs as RDS files
    
    file_name <- paste0("design_proj_", proj_id, "_caliper_", caliper_val, "_dist_", dist_use, "_replace_", match_w_replacement, ".RDS")
    saveRDS(d_matched_out, file = file.path(d_matched_dir, file_name))
    saveRDS(step_model, file = file.path(prop_score_dir, file_name))
    saveRDS(match_out_dist, file = file.path(match_out_dir, file_name))
  })
  
  return(try_output)  # Return the result of the try block
}

# Make a control table to list successful/pending runs
do_tab$file_name <- paste0("design_proj_", do_tab$proj_id, "_caliper_", do_tab$caliper_val, "_dist_", do_tab$dist_use, "_replace_", do_tab$match_w_replacement, ".RDS")

do_tab$d_matched_dir =  file.exists(file.path(d_matched_dir, do_tab$file_name))
do_tab$prop_score_dir =  file.exists(file.path(prop_score_dir, do_tab$file_name))
do_tab$match_out_dir =  file.exists(file.path(match_out_dir, do_tab$file_name))

do_tab %>% 
  select(-file_name) %>% 
  write_csv(file.path("data", "02_propensity_score_subclasses_outputs_tracker.csv"))

# Stop the parallel cluster
stopCluster(cl)
