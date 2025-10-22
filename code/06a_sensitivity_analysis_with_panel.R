# Load required preamble
source(file.path("code", "00_preamble.R"), echo = TRUE)

library(ggplot2) 

# Load matched dataset and results
d <- readRDS(file = file.path(dir_output, paste0("all_dat_matched_alpha_", alpha_use, ".RDS")))
combined_lm_ps_weight_adj_results <- readRDS(file = file.path(dir_output, "combined_lm_ps_weight_adj_results.RDS"))

# Load claims data and join to project table
claims <- read_csv(file = file.path("data", "avoided-defor-ha.csv")) %>%
  mutate(project = as.character(vcs_id))

# Join claims data with project table, calculate implied ATE, and sort
proj_tab <- proj_tab %>%
  left_join(claims %>% select(c("project", "years_n", "avoided_verra")), by = "project") %>%
  mutate(implied_ate = 100 * avoided_verra / area_ha / years_n) %>%
  arrange(implied_ate)

# Load matching proportions
ordl <- readRDS(file = file.path(dir_output, "ordl.RDS"))
match_prop_tab <- ordl[[dist_use]][[as.character(alpha_use)]]

# Save matched proportion table to CSV
write.csv(match_prop_tab, file = file.path(dir_output, "tmp_matched_prop.csv"))

# Filter projects with low matching proportions
projects_with_low_matched_prop <- match_prop_tab %>%
  filter(match_prop_tab[, paste0("prop_matched_alpha_", alpha_use), drop = T] < prop_matched_thresh & match_prop_tab[, paste0("prop_matched_alpha_", alpha_use), drop = T] >0) %>%
  pull(proj_id)

not_matched <- match_prop_tab %>%
  filter( match_prop_tab[, paste0("prop_matched_alpha_", alpha_use), drop = T] ==0) %>%
  pull(proj_id)

# Filter and prepare project data for analysis
proj_comp <- proj_tab %>%
  filter(!is.na(implied_ate))

# Initialize variables for iteration and plotting
k_try <- c(0, 1, 2, 3)
res_plot <- res_plot2 <- tibble()

# List of observed confounders for sensitivity analysis
var_benchmark <- list(
  dist_degra = "dist_degra",
)
var_benchmark_plot_order <- c("none", "dist_degra")

# Loop over each project in the comparison set
for (proj in proj_comp$proj_id) {

# Load VERRA claimed ATE
ate_yr_to_compare <- proj_tab %>%
    filter(proj_id == proj) %>%
    pull(implied_ate)
  
  # Load cross-sectional analyses results (focus on matched GIDs)
  d_sub <- d %>%
    filter(proj_id == proj, !subclass_remove) %>%
    mutate(difference_yr = 100 * outcome_prop / 5)

 # Load panel-format datasets (all treat and control)
 panel_data_file <- file.path(dir_panel_data, paste0("panel_", proj, ".RDS"))
 panel <- readRDS(panel_data_file)
 time_treat_min = -5

# apply QC checks
 panel <- panel %>% 
    filter(!is.na(z), !is.na(gid), !is.na(time_treat), !is.na(treat), !is.na(post)) %>%
    filter(time_treat >= time_treat_min) %>%
    mutate(time_treat_fac = factor(time_treat),
                gid = as.integer(gid))
 
 # to %/year
 panel$z_perc = panel$z * 100
 # Add offset parameter
panel$offset_yr <- ifelse(panel$treat == "1", ate_yr_to_compare, 0)

# Create subset panel dataset to the matched GIDs
panel_sub <- panel %>% filter(gid %in% d_sub$gid)
panel_sub <- panel_sub %>% mutate(gid_fac = factor(gid))

# Now create dataset with random selection of controls for comparison
# Sample control units to match treated units
# Set seed for reproducibility
  set.seed(1234)
  d_treat <- panel %>% filter(treat == "1") 
  d_control <- panel %>% filter(treat == "0") 
  n_treat_gid <- length(unique(d_treat$gid))

  d_control <- d_control %>%
    distinct(gid) %>%       
    slice_sample(n = n_treat_gid) %>% 
    inner_join(d_control, by = "gid") 

  d_unmatched <- bind_rows(d_treat, d_control)
  d_unmatched <- d_unmatched %>% mutate(gid_fac = factor(gid)) # apply to all
  
 # FIT PANEL MODELS ---
                            
# Define panel analysis parameters
xvars = c("dist_degra" )
cluster_choice = c("gid_fac", "time_treat_fac")
fixefs = c("gid_fac", "time_treat_fac")

m_sub <- panel_data_fit_one_project(panel_sub,
                                         y = "z",
                                         xvars = xvars,
                                         fixefs = fixefs,
                                         cluster = cluster_choice,
                                          offset = "offset_yr")

m <- panel_data_fit_one_project(d_unmatched,
                                         y = "z",
                                         xvars = xvars,
                                         fixefs = fixefs,
                                         cluster = cluster_choice,
                                          offset = 
                                          "offset_yr")     

unmatched_coef_tab <- summary(m)$coefficients
all_vars_available_to_benchmark <- names(unmatched_coef_tab)
}