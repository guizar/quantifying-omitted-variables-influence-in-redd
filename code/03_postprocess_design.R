# Load preamble settings and functions
source(file.path("code", "00_preamble.R"), echo = TRUE)

# Control for whether to repopulate `do_tab`
repop_do_tab <- TRUE

if (repop_do_tab) {
  
  # Initialize columns in `do_tab` to store treatment and matching results
  do_tab[, c("n_treat_tot", "n_treat_matched")] <- NA
  
  # Initialize columns for each alpha value from `qqplot_alpha_vec`
  for (alpha in qqplot_alpha_vec) {
    do_tab[, paste0("n_treat_ok_alpha_", alpha)] <- NA
    do_tab[, paste0("prop_matched_alpha_", alpha)] <- NA
  }
  
  # Loop through each project to process matching results
  for (j in 1:nrow(do_tab)) {
    print(j)
    proj_curr <- do_tab$proj_id[j]  # Current project ID
    caliper_val <- do_tab$caliper_val[j]  # Current caliper value
    dist_use <- do_tab$dist_use[j]  # Distance metric used
    match_w_replacement <- do_tab$match_w_replacement[j] # repl
    d_qc <- readRDS(file = file.path(dir_qc_data, paste0("qc_data_", proj_curr, ".RDS")))  # Load quality-controlled data
    
    # Load matching results for the current project
    file_name <- paste0("design_proj_", proj_curr, "_caliper_", caliper_val, "_dist_", dist_use, "_replace_", match_w_replacement, ".RDS")
    # try, if not break:
     if (!file.exists(file.path(d_matched_dir, file_name))) {
    message("RDS file does not exist:", file_name)
    next  # Move to the next iteration of the loop
  }
    design_in <- readRDS(file = file.path(d_matched_dir, file_name))
    d_matched <- design_in$d_matched  # Matched dataset
    
    # Store total number of treated units and matched treated units
    do_tab$n_treat_tot[j] <- sum(d_qc$treat == 1)
    do_tab$n_treat_matched[j] <- sum(d_matched$treat == 1)
    
    # For each alpha, calculate the number of treated units matched and their proportion
    for (alpha in qqplot_alpha_vec) {
      subclass_remove <- design_in$subclass_remove_list[[as.character(alpha)]]  # Subclasses removed for current alpha
      d_matched$removed <- d_matched$subclass %in% subclass_remove  # Mark removed subclasses
      do_tab[j, paste0("n_treat_ok_alpha_", alpha)] <- sum(d_matched$treat == 1 & !d_matched$removed)  # Count remaining treated units
      do_tab[j, paste0("prop_matched_alpha_", alpha)] <- do_tab[j, paste0("n_treat_ok_alpha_", alpha)] / do_tab$n_treat_tot[j]  # Calculate proportion
    }
  }
  
  # Save updated `do_tab` with matching success results
  saveRDS(do_tab, file = file.path(dir_output, "do_tab_with_matching_success.RDS"))
  
} else {
  # Load previously saved `do_tab` if not repopulating
  do_tab <- readRDS(file = file.path(dir_output, "do_tab_with_matching_success.RDS"))
}


# Initialize list to store best matches by distance metric and alpha
ordl <- list()

# Loop through each distance metric (and "any" for all distances combined)
for (dist_curr in c(dist_use_vec, "any")) {
  ordl[[dist_curr]] <- list()  # Create list for current distance metric
  
  # Filter `do_tab` based on distance metric or use the full table for "any"
  if (dist_curr == "any") {
    do_tab_sub <- do_tab
  } else {
    do_tab_sub <- do_tab %>% filter(dist_use == dist_curr)
  }
  
  # For each alpha, find the best matches by project
  for (alpha in qqplot_alpha_vec) {
    best_by_proj <- do_tab_sub[order(do_tab_sub$proj_id, -do_tab_sub[, paste0("prop_matched_alpha_", alpha)]), ]  # Order by proportion matched
    best_by_proj %<>% 
      group_by(proj_id) %>% 
      slice(1) %>% 
      ungroup()  # Select best match for each project
    ordl[[dist_curr]][[as.character(alpha)]] <- best_by_proj[order(unlist(best_by_proj[, paste0("prop_matched_alpha_", alpha)])), ]  # Store ordered matches
  }
}

# Save the ordered list of matches
saveRDS(ordl, file = file.path(dir_output, "ordl.RDS"))


# Load the ordered matches and prepare the final dataset
ordl <- readRDS(file = file.path(dir_output, "ordl.RDS"))
all_dat <- tibble()  # Initialize empty tibble to store matched data

# Select the design table for the chosen distance metric and alpha
design_tab <- ordl[[dist_use]][[as.character(alpha_use)]]

# Loop through each project and bind matched data to `all_dat`
for (j in 1:nrow(proj_tab)) {
  proj_curr <- proj_tab$proj_id[j]  # Current project ID
  
  # Get the best design row for the current project
  design_row <- design_tab %>% filter(proj_id == proj_curr)
  caliper_val <- design_row$caliper_val  # Get caliper value for the project
  
  # Load the matched dataset for the project
  file_name <- paste0("design_proj_", proj_curr, "_caliper_", caliper_val, "_dist_", dist_use, "_replace_", match_w_replacement, ".RDS")
  design_in <- readRDS(file = file.path(d_matched_dir, file_name))
  d_matched <- design_in$d_matched
  
  # Mark and remove the subclasses that did not meet the alpha threshold
  subclass_remove <- design_in$subclass_remove_list[[as.character(alpha_use)]]
  d_matched %<>% mutate(subclass_remove = subclass %in% subclass_remove)
  
  # Append the matched data to the final dataset
  all_dat %<>% bind_rows(d_matched)
}

# Calculate propensity score weights for each unit and add to `all_dat`
all_dat %<>% mutate(ps_weights = ifelse(treat == 1, 1 / raw_ps_score, 1 / (1 - raw_ps_score)))

# Check structure of the final dataset
str(all_dat)

# Save the final dataset with all matched data
saveRDS(all_dat, file = file.path(dir_output, paste0("all_dat_matched_alpha_", alpha_use, ".RDS")))














