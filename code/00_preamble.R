# Load necessary libraries
suppressPackageStartupMessages(library(tidyverse))
library(dplyr)
library(knitr)
library(magrittr)
library(doParallel)
library(ggplot2)
library(qqplotr)
library(broom)
library(arrangements) # for permutations
library(pander) # to render tables
library(rcartocolor)
library(randomForest)
library(monochromeR)
library(boot)

# Source custom functions from the "code/functions/" directory
fn_files_to_source <- list.files(file.path("code", "functions"), full.names = TRUE)
for (file_curr in fn_files_to_source) {
  source(file_curr)  # Source each function file
}

# Define pth to data directory and retrieve list of filenames (without extensions)
filenames <- list.files(file.path("data", "matched_sets"), full.names = FALSE)
filenames <- str_remove(filenames, "\\.csv")  # Remove .csv extension

# define directories -- defining here as separate data paths for the server
design_out_dir <- file.path("data", "output", "design_out_dir")
dir_qc_data <- file.path("data", "output", "post_qc_unmatched")
dir_ps <- file.path("data", "output", "stage1_propensity_score_results")
dir_matched_data <- file.path("data", "output", "post_qc_matched")
d_matched_dir <- file.path("data", "output", "design_out_dir", "d_matched")
prop_score_dir <- file.path("data", "output", "design_out_dir", "prop_score_model")
match_out_dir <- file.path("data", "output", "design_out_dir", "matchit_out")
dir_analysis_outputs <- file.path("data", "output", "analysis_outputs")

# specific to the initial matching exploration:
match_out_init_dir <- file.path("data", "output", "design_out_dir", "matchit_out_init")
match_dd_dir <- file.path("data", "output", "design_out_dir", "matchit_dd_init")

# define local directories -- files small in size that don't need to be stored in the server
dir_output <- file.path("data", "output")
dir_figures <- file.path("figures")
dir_tables <- file.path("tables")

# Create necessary directories if they don't exist
dir.create(d_matched_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(prop_score_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(match_out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(design_out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(dir_ps, showWarnings = FALSE, recursive = TRUE)
dir.create(dir_qc_data, showWarnings = FALSE, recursive = TRUE)
dir.create(dir_matched_data, showWarnings = FALSE, recursive = TRUE)
dir.create(dir_analysis_outputs, showWarnings = FALSE, recursive = TRUE)
dir.create(dir_output, showWarnings = FALSE, recursive = TRUE)
dir.create(dir_figures, showWarnings = FALSE, recursive = TRUE)
dir.create(dir_tables, showWarnings = FALSE, recursive = TRUE)
dir.create(match_out_init_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(match_dd_dir, showWarnings = FALSE, recursive = TRUE)

# Set full data directory (update based on the version of data being used)
full_data_dir <- file.path("data", "full_data_2024-08-11")
proj_tab_file <- file.path("data", "proj_tab.RDS")

# If project table does not exist, create it from full data
if (!file.exists(proj_tab_file)) {
  f_all <- list.files(full_data_dir, pattern = "*.csv")  # List all files in full data directory
  # file.info(list.files(full_data_dir, pattern = "*.csv", full.names = TRUE))  # Get file info
  splits <- strsplit(f_all, split = "_")
  
  # Create project table with unique project and country combinations
  proj_tab <- unique(tibble(country = gsub(".csv", "", sapply(splits, function(x) x[2])),
                            project = sapply(splits, function(x) x[[1]]))) %>%
    mutate(long_file = paste(project, country, "long.csv", sep = "_")) %>%
    mutate(summ_file = paste(project, "_", country, ".csv", sep = ""))
  
  # Add file sizes and sort by size
  proj_tab %<>%
    mutate(size = file.info(file.path(full_data_dir, proj_tab$long_file))$size) %>%
    arrange(size) %>%
    mutate(proj_id = paste0(country, "_", project))
  
  # filter weird ID
  proj_tab = proj_tab %>% filter(proj_id!='data_full')

  # Merge additional project information from vcs-info.csv
  proj_info <- read_csv(file = file.path("data", "vcs-info.csv")) %>%
    mutate(project = vcs_id)
  proj_tab %<>%
    left_join(proj_info %>% select(c("project", "area_ha", "project_name", "syear")), by = "project")
  
  # Save the project table as an RDS file
  saveRDS(proj_tab, file = proj_tab_file)
  
} else {
  # If project table exists, load it
  proj_tab <- readRDS(file = proj_tab_file)
}

# Define core variables and additional trend/baseline variables
core_vars <- c("dist_degra", "mean_slp", "mean_access", "mean_ele")
trend_vars <- c("pre_short_term_trend", "pre_short_term_trend_prop", "pre_long_term_trend", "pre_long_term_trend_prop")
baseline_vars <- c("area_forest_t_1990", "area_forest_t_minus_6", "area_forest_t_zero")
trend_baseline_vars_for_matching <- c(trend_vars, "area_forest_t_minus_6")
all_explanatory_vars_for_matching <- c(core_vars, trend_baseline_vars_for_matching)

# Option to explore rank-deficient designs (if set to TRUE)
explore_rank_deficient_designs <- FALSE
if (explore_rank_deficient_designs) {
  # Comment: Include only five pre-treatment variables to avoid rank deficiency
  trend_vars <- c("pre_short_term_trend", "pre_short_term_trend_prop", "pre_long_term_trend", "pre_long_term_trend_prop", "area_forest_t_minus_6")
  
  # Define full set of explanatory variables for propensity score estimation
  all_explanatory_vars <- c("dist_degra", "mean_slp", "mean_access", "mean_ele", trend_vars)
  ps_score_form <- reformulate(response = "treat", termlabels = c("-1", all_explanatory_vars))
  
  # Create model matrix and check rank deficiency
  X <- model.matrix(ps_score_form, model.frame(ps_score_form, d_qc))
  qr(X)$rank  # Check rank of the model matrix
  ncol(X)  # Number of columns in the model matrix
  length(attr(terms(ps_score_form), "term.labels"))  # Number of terms in the formula
  
  # Subset formula for a simpler model and create model matrix
  sub_form <- as.formula(treat ~ pre_short_term_trend + pre_long_term_trend + area_forest_t_1990 + area_forest_t_minus_6)
  X_sub <- model.matrix(sub_form, model.frame(sub_form, d_qc))
  head(X_sub)
}

# Define propensity score formula and outcome formulas (adjusted and unadjusted)
ps_score_form <- reformulate(response = "treat", termlabels = all_explanatory_vars_for_matching)
adjusted_outcome_form <- reformulate(response = "outcome_prop", termlabels = c("treat", all_explanatory_vars_for_matching))
unadjusted_outcome_form <- reformulate(response = "outcome_prop", termlabels = "treat")

# Define matching parameters and thresholds
caliper_val_vec <- c(0.1, 0.2, 0.5, 1)  # Different caliper values for matching
dist_use_vec <- c("glm", "mahalanobis", "randomforest")  # Distance metrics for matching
is_match_w_replacement <- TRUE  # Allow matching with replacement
qqplot_alpha_vec <- c(0.05, 0.05 / nrow(proj_tab))  # Alpha values for QQ plots
alpha_use <- 0.05 / nrow(proj_tab)  # Alpha level adjusted for multiple comparisons
dist_use <- "randomforest"
prop_matched_thresh <- 0.8  # Threshold for the proportion of matched units

# Create combinations of projects, caliper values, and distance metrics
proj_all <- proj_tab$proj_id  # List of all project IDs
do_tab <- expand.grid(proj_id = proj_all, caliper_val = caliper_val_vec, dist_use = dist_use_vec, match_w_replacement = is_match_w_replacement, stringsAsFactors = FALSE)  # Grid of matching parameters
do_tab$id = 1:nrow(do_tab)

# -----------------------------------------------
# DEFINING PARAMS FOR INITIAL MATCHING EXPLORATION
# ------------------------------------------------

proj_tab <- proj_tab %>% mutate(vcs_id = paste0(project))

# To be added to a control_parameters script
raw_term_trend = c('pre_short_term_trend', 'pre_long_term_trend')
prop_term_trend = c("pre_short_term_trend_prop", "pre_long_term_trend_prop")
matching_trend_vars <- c(raw_term_trend, prop_term_trend)
matching_base_vars <- c("dist_degra", "mean_slp", "mean_access", "mean_ele")

Xbase = paste0(c(matching_trend_vars, matching_base_vars),collapse=', ')
XFor = c('area_forest_t_1990','area_forest_t_minus_6')

# -- COMBINATION OF COVARIATES, WITH/WITH OUT FOREST AREA
n_vars = length(XFor)

# get all the indices permutations
all_perms = combinations(n_vars, n_vars, layout = "list", replace = T)

# reduce to unique arrays
all_perms_u = lapply(all_perms, function(x) {unique(x)})
all_perms_u = lapply(all_perms_u, function(x) {sort(unlist(x))})
all_perms_u = unique(all_perms_u)
all_perms_u = all_perms_u[1:3]

# final list of covars
covars_ls = map(all_perms_u, function(x){XFor[x]})
covars_ls = map(covars_ls, function(x){str_flatten(x,', ')})
covariate_combinations = unlist(covars_ls)
covariate_combinations = paste(Xbase, covariate_combinations,sep=', ')
covariate_combinations = c(Xbase,covariate_combinations)

# --- Construct matching specifications table
# params matching params

caliper_val_vec <- c(.1, .2, .5, 1)
dist_use_vec <- c("glm", "mahalanobis","randomforest")
is_match_w_replacement <- c(TRUE,FALSE)
qqplot_alpha_vec <- c(0.05, .05 / nrow(proj_tab))
alpha_use <- .05 / nrow(proj_tab)
prop_matched_thresh <- 0.8
proj_all <- proj_tab$proj_id

# DO MATCHING ITERATIONS TABLE
do_tab_match_init <- 
expand.grid(
  proj_id = proj_all, 
  covars = covariate_combinations,
  caliper_val = caliper_val_vec, 
  dist_use = dist_use_vec, 
  match_w_replacement = is_match_w_replacement,
  stringsAsFactors = FALSE)
do_tab_match_init$id  = 1:nrow(do_tab_match_init)
do_tab_match_init %<>% left_join (proj_tab %>% select(vcs_id, proj_id))

# WRITE REFERENCE CSVS
do_tab_match_init %>% write_csv(file.path("data", "do_tab_match_init.csv"))
do_tab %>% write_csv(file.path("data", "do_tab.csv"))
proj_tab %>% write_csv(file.path("data", "proj_tab.csv"))

# -----------------------------------------------------------------
# Declare project labels according to project's inclusion in this study, declare add label and colour for `80% of plots matched`
# -----------------------------------------------------------------
inclusion_lab = c('<80% evergreen forest cover','Insufficient temporal\n records', 'Not\nmatched', '<80% of plots matched','Included')
map_cols= carto_pal(5, "Vivid")
map_cols[5] <- '#000000'
names(map_cols) <- inclusion_lab
under_80_label <- inclusion_lab[4]
axis_text_color_dimmed <- map_cols[under_80_label]
