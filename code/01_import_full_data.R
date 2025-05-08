# Load preamble settings and functions
source(file.path("code", "00_preamble.R"), echo = TRUE)

# Set up multi-core processing
multi_core <- TRUE
n_cores <- if (multi_core) min(25, parallel::detectCores()) else 1 # Use max cores available (or 25 max) if multi_core is TRUE, otherwise 1 core
cl <- parallel::makePSOCKcluster(n_cores)  
doParallel::registerDoParallel(cl)  # Register the parallel backend for foreach

# Perform parallel processing over all projects in proj_tab
outl <- foreach::foreach(j = 1:nrow(proj_tab),
                         .verbose = TRUE,  # Print progress information during execution
                         .packages = c("tidyverse", "dplyr", "magrittr"),  # Load necessary packages
                         .errorhandling = "pass"  # Continue execution even if an error occurs
) %dopar% {

  # Retrieve current project information and parameters from do_tab
  proj_curr <- proj_tab$proj_id[j]  # Current project ID
  file_name <- paste0("qc_data_", proj_tab$proj_id[j], ".RDS")

  # Check if output files exist,
  if (file.exists(file.path(dir_qc_data, file_name))) {
  cat(sprintf("File exists for project %s. Skipping.\n", proj_curr))
    return(NULL)
  }
  
  # Create an n_years_post object to account for proj's 'PL1748' reduce n years
  n_years_post <- ifelse(proj_curr!='KHM_PL1748',5,4)

  # Read in the long and short dataset for the current project
  long <- read_csv(file = file.path(full_data_dir, proj_tab$long_file[j]))  # Read long format data
  short <- read_csv(file = file.path(full_data_dir, proj_tab$summ_file[j]))  # Read short summary data
  
  # Add country and project identifiers to the long dataset
  long %<>% 
    mutate(country = proj_tab$country[j]) %>%  # Add country
    mutate(project = proj_tab$project[j])  # Add project
  
  # Summarize data to create key variables for each project
  d <- long %>%
    group_by(country, project, gid) %>%  # Group by country, project, and gid (unique identifier)
    arrange(year) %>%  # Arrange by year
    summarise(
      outcome = sum(lag_arealoss[time_treat >= 1 & time_treat <= 5]),  # Deforestation outcome within 5 years after treatment
      area_forest_t_1990 = areaforest[year == 1990],  # Forest area in 1990
      area_forest_t_minus_6 = areaforest[time_treat == -6],  # Forest area 6 years before treatment
      area_forest_t_zero = areaforest[time_treat == 0],  # Forest area at the time of treatment
      area_loss_t_minus_6 = arealoss[time_treat == -6], 
      area_loss_t_zero = arealoss[time_treat == 0],
      area_loss_t_plus_5 = arealoss[time_treat == n_years_post], # should be the same as outcome
      pre_long_term_trend = area_forest_t_1990 - area_forest_t_zero,  # Long-term forest trend (1990 to treatment)
      pre_short_term_trend = area_forest_t_minus_6 - area_forest_t_zero,  # Short-term forest trend (6 years before treatment)
      total_loss = sum(lag_arealoss),  # Total forest loss over the period
      outcome_prop = ifelse(area_forest_t_minus_6 == 0, ifelse(outcome == 0, 0, Inf), outcome / area_forest_t_minus_6),  # Outcome proportion
      pre_short_term_trend_prop = ifelse(area_forest_t_minus_6 == 0, 
                                         ifelse(pre_short_term_trend == 0, 0, Inf), pre_short_term_trend / area_forest_t_minus_6),  # Proportional short-term trend
      pre_long_term_trend_prop = ifelse(area_forest_t_1990 == 0, 
                                        ifelse(pre_long_term_trend == 0, 0, Inf), pre_long_term_trend / area_forest_t_1990),  # Proportional long-term trend
      total_prop = ifelse(area_forest_t_minus_6 == 0, ifelse(total_loss == 0, 0, Inf), total_loss / area_forest_t_minus_6),  # Proportional total loss
      dist_degra = dist_degra[time_treat == 0],  # Distance to degradation at time of treatment
      .groups = "keep"  # Retain groupings for further operations
    ) %>%
    left_join(short, by = "gid") %>%  # Join with the short summary data by gid
    ungroup() %>%  # Ungroup the dataset
    mutate(proj_id = paste(country, project, sep = "_"))  # Create a project ID by concatenating country and project
  
  # Quality control: Remove entries with missing or implausible values
  d_qc <- d %>% 
    filter(total_prop <= 1) %>%  # Remove entries with total loss > 100%
    filter(!is.na(dist_degra)) %>%  # Remove rows with missing distance to degradation
    filter(!is.na(mean_slp)) %>%  # Remove rows with missing slope data
    filter(!is.na(mean_access)) %>%  # Remove rows with missing access data
    filter(!is.na(mean_ele))  # Remove rows with missing elevation data
  
  # Scale selected variables for further analysis
  vars_scale <- c("dist_degra", "mean_slp", "mean_access", "mean_ele", 
                  "pre_short_term_trend", "pre_short_term_trend_prop", 
                  "pre_long_term_trend", "pre_long_term_trend_prop", 
                  "area_forest_t_1990", "area_forest_t_minus_6", "area_forest_t_zero",
                  "area_loss_t_minus_6", "area_loss_t_zero", "area_loss_t_plus_5")  # Variables to scale

  # Store original values for some variables to conduct post-matching DDs 
  d_qc <- d_qc %>% 
    mutate(across(all_of(vars_scale), 
                  .fns = list(unscl = ~ .), 
                  .names = "{.col}_unscl"))
  
  for (var_curr in vars_scale) {
    d_qc[[var_curr]] <- c(scale(d_qc[[var_curr]]))  # Standardize each variable
  }
  
  # Save the quality-controlled dataset as an RDS file
  saveRDS(d_qc, file = file.path(dir_qc_data, paste0("qc_data_", proj_tab$proj_id[j], ".RDS")))
  
}

# Stop the parallel processing cluster
stopCluster(cl)


# --------------------------------------
# Summarise project-level covar values 
# --------------------------------------

d_sum  <- data.frame()
d_sum_counts  <- data.frame()

for (proj_id in proj_tab$proj_id) {
d_qc <- readRDS(file = file.path(dir_qc_data, paste0("qc_data_", proj_id, ".RDS")))
# 'reinforce' treat as a binary variable
d_qc$treat <- as.integer(d_qc$treat)

vars_scale <- paste0(c("dist_degra", "mean_slp", "mean_access", "mean_ele", 
                "pre_short_term_trend", "pre_short_term_trend_prop", 
                "pre_long_term_trend", "pre_long_term_trend_prop", 
                "area_forest_t_1990", "area_forest_t_minus_6", "area_forest_t_zero",
                "area_loss_t_minus_6", "area_loss_t_zero", "area_loss_t_plus_5"),'_unscl')

d_sum_j <- d_qc %>% 
  filter(treat == 1) %>% 
  select(all_of(vars_scale)) %>%
  rename_with(~ gsub("_unscl$", "", .x), all_of(vars_scale))


plot_level <- c("dist_degra", "mean_slp", "mean_access", "mean_ele")
d_sum_j_plot <- d_sum_j %>% select(all_of(plot_level)) %>% summarise(across(everything(), list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)))) 

project_level <- c("pre_short_term_trend", 
                "pre_long_term_trend", 
                "area_forest_t_1990", "area_forest_t_minus_6", "area_forest_t_zero",
                "area_loss_t_minus_6", "area_loss_t_zero", "area_loss_t_plus_5")

d_sum_j_project <- d_sum_j %>% select(all_of(project_level)) %>% summarise(across(everything(), list(sum = ~sum(.x, na.rm = TRUE)))) %>%
mutate(pre_short_term_trend_prop = ifelse(area_forest_t_minus_6_sum == 0, 
                                         ifelse(pre_short_term_trend_sum == 0, 0, Inf), pre_short_term_trend_sum / area_forest_t_minus_6_sum),  # Proportional short-term trend
      pre_long_term_trend_prop = ifelse(area_forest_t_1990_sum == 0, 
                                        ifelse(pre_long_term_trend_sum == 0, 0, Inf), pre_long_term_trend_sum / area_forest_t_1990_sum),  # Proportional long-term trend
                                        )

d_sum_j <- bind_cols(d_sum_j_plot,d_sum_j_project) %>%
# pivot_longer(cols = everything(), names_to = c(".value", "stat"), names_sep = "_(?=[^_]+$)") %>%
mutate(proj_id=proj_id)
d_sum <- bind_rows(d_sum,d_sum_j)

# load counts
d_counts <- d_qc %>% group_by(treat) %>% tally() %>% mutate(proj_id=proj_id)
d_sum_counts <- bind_rows(d_sum_counts,d_counts)
}

d_sum %>% write_csv(file.path("data", "proj_covar_sums.csv"))
d_sum_counts %>% write_csv(file.path("tables", "proj_treat_counts.csv"))
