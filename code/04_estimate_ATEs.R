# Load preamble settings and functions
source(file.path("code", "00_preamble.R"), echo = TRUE)

# Load matched data with specified alpha
d <- readRDS(file = file.path(dir_output, paste0("all_dat_matched_alpha_", alpha_use, ".RDS")))

# Set control flags for model running and spatial model fitting
run_models <- TRUE
fit_spatial <- TRUE

# Get unique project IDs
proj_id_unique <- proj_tab$proj_id

# Summarize pi (proportion of non-zero outcomes) and mu (mean of non-zero outcomes) for each project
summs <- d %>%
  group_by(proj_id) %>%
  summarise(pi_est = mean(outcome_prop != 0),  # Proportion of non-zero outcomes
            mu_est = mean(outcome_prop[outcome_prop != 0])) %>%  # Mean of non-zero outcomes
  ungroup()

# Calculate prior mean and standard deviation for mu (to be used in models)
mu_pri_mn <- mean(summs$mu_est, na.rm = TRUE)
mu_pri_sd <- sd(summs$mu_est, na.rm = TRUE)

# If running models, proceed with parallel processing
if (run_models) {
  # Load required library for parallel processing
  library(doParallel)
  
  # Set up parallel cluster with 20 cores if fitting spatial models, otherwise 1 core
  cl <- parallel::makePSOCKcluster(ifelse(fit_spatial, 20, 1))
  doParallel::registerDoParallel(cl)  # Register parallel backend
  
  # Run the models in parallel for each project
  outl <- foreach::foreach(proj_id_curr = proj_id_unique,
                           .verbose = TRUE,  # Print progress
                           .packages = c("tidyverse", "dplyr", "magrittr"),
                           .errorhandling = "pass"  # Continue on error
  ) %dopar% {
    # Filter data for the current project and remove excluded subclasses
    d_sub <- d %>% 
      filter(proj_id == proj_id_curr) %>%
      filter(!subclass_remove)
    
    subclass_unique <- unique(d_sub$subclass)  # Get unique subclasses
    subclass_weights <- table(d_sub$subclass) / nrow(d_sub)  # Calculate subclass weights
    
    #########################################################
    # Fit categorical/quantitative model with propensity score subclasses
    cat_quant_result <- fit_simple_cat_quant_model(d_sub = d_sub, 
                                                   mu_pri_mean = mu_pri_mn, 
                                                   mu_pri_sd = mu_pri_sd)
    
    #########################################################
    # Fit basic linear model without any adjustment after matching 
    lm_result_simple <- fit_simple_lm_subclass(d_sub = d_sub,
                                               formula = unadjusted_outcome_form,
                                               method = "simple")
    
    #########################################################
    # Fit basic linear model with linear adjustment after matching 
    lm_result_simple_adjusted <- fit_simple_lm_subclass(d_sub = d_sub,
                                                        formula = adjusted_outcome_form,
                                                        method = "simple")
    
    #########################################################
    # Fit basic linear model with propensity score subclasses
    lm_result <- fit_simple_lm_subclass(d_sub = d_sub,
                                        formula = outcome_prop ~ treat,
                                        method = "ps_subclass")
    
    #########################################################
    # Fit basic linear model with propensity score weighting
    lm_result_ps_weights <- fit_simple_lm_subclass(d_sub = d_sub,
                                                   formula =  unadjusted_outcome_form,
                                                   method = "ps_weights",
                                                   ps_weights = d_sub$ps_weights)
    
    #########################################################
    # Fit basic linear model with propensity score subclasses also adjusting for covariates within subclasses
    lm_adj_result <- fit_simple_lm_subclass(d_sub = d_sub,
                                            formula = adjusted_outcome_form,
                                            method = "ps_subclass")
    
    #########################################################
    # Fit basic linear model with propensity score weighting and adjusting for covariates
    lm_ps_weight_adj_result <- fit_simple_lm_subclass(d_sub = d_sub,
                                                      formula =  adjusted_outcome_form,
                                                      method = "ps_weights",
                                                      ps_weights = d_sub$ps_weights)
    
    # Compile all results into a list
    out <- list(cat_quant_result = cat_quant_result,
                lm_result = lm_result,
                lm_result_simple = lm_result_simple,
                lm_result_simple_adjusted = lm_result_simple_adjusted,
                lm_adj_result = lm_adj_result,
                lm_ps_weight_adj_result = lm_ps_weight_adj_result,
                lm_result_ps_weights = lm_result_ps_weights)
    
    # Optionally fit a spatial model if `fit_spatial` is TRUE
    if (fit_spatial) {
      lme_adj_result <- fit_spatial_lme_subclass(d_sub = d_sub,
                                                 formula = adjusted_outcome_form)
      out$lme_adj_result <- lme_adj_result  # Add spatial model result to output list
    }
    
    # Save results for the current project
    saveRDS(out, file = file.path(dir_analysis_outputs, paste0("model_fits_spatial_", fit_spatial, "_", proj_id_curr, ".RDS")))
    return(out)  # Return the results for the current project
  }
  
  # Stop the parallel cluster after processing
  parallel::stopCluster(cl)
  
  # Save all model results
  saveRDS(outl, file = file.path(dir_output, paste0("model_fits_spatial_", fit_spatial, ".RDS")))
  
} else {
  # If models are not being run, load previously saved results
  outl <- readRDS(file = file.path(dir_output, paste0("model_fits_spatial_", fit_spatial, ".RDS")))
}

#########################################################
# Combine and save linear model results across projects

combined_lm_results_simple <- bind_rows(lapply(outl, function(x) x$lm_result_simple))
combined_lm_results_simple_adjusted <- bind_rows(lapply(outl, function(x) x$lm_result_simple_adjusted))
combined_lm_results <- bind_rows(lapply(outl, function(x) x$lm_result))
combined_lm_adj_results <- bind_rows(lapply(outl, function(x) x$lm_adj_result))
combined_lm_ps_weight_adj_results <- bind_rows(lapply(outl, function(x) x$lm_ps_weight_adj_result))
combined_lm_ps_weights_results <- bind_rows(lapply(outl, function(x) x$lm_result_ps_weights))
combined_cat_quant_results <- bind_rows(lapply(outl, function(x) x$cat_quant_result))

# Save combined model results
saveRDS(combined_lm_results_simple, file = file.path(dir_output, "combined_lm_results_simple.RDS"))
saveRDS(combined_lm_results_simple_adjusted, file = file.path(dir_output, "combined_lm_results_simple_adjusted.RDS"))
saveRDS(combined_lm_results, file = file.path(dir_output, "combined_lm_results.RDS"))
saveRDS(combined_lm_adj_results, file = file.path(dir_output, "combined_lm_adj_results.RDS"))
saveRDS(combined_lm_ps_weight_adj_results, file = file.path(dir_output, "combined_lm_ps_weight_adj_results.RDS"))
saveRDS(combined_lm_ps_weights_results, file = file.path(dir_output, "combined_lm_ps_weights_results.RDS"))
saveRDS(combined_cat_quant_results, file = file.path(dir_output, "combined_cat_quant_results.RDS"))

# If spatial models were fitted, combine and save those results as well
if (fit_spatial) {
  combined_lme_adj_results <- bind_rows(lapply(outl, function(x) x$lme_adj_result))
  saveRDS(combined_lme_adj_results, file = file.path(dir_output, "combined_lme_adj_results.RDS"))
}
