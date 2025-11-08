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
  dist_degra = "dist_degra"
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
 
# Identify GIDs where z exceeds 1 at any time point and remove all observations for those GIDs
gids_to_remove <- panel %>%
  filter(z > 1) %>%
  distinct(gid) %>%
  pull(gid)

panel <- panel %>%
  filter(!gid %in% gids_to_remove)

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
xvars = NULL 
cluster_choice = c("gid_fac", "time_treat_fac")
fixefs = c("gid_fac", "time_treat_fac")

m_matched <- panel_data_fit_one_project(panel_sub,
                                         y = "z",
                                         xvars = xvars,
                                         fixefs = fixefs,
                                         cluster = cluster_choice,
                                          offset = "offset_yr")

m_unmatched <- panel_data_fit_one_project(d_unmatched,
                                         y = "z",
                                         xvars = xvars,
                                         fixefs = fixefs,
                                         cluster = cluster_choice,
                                          offset = 
                                          "offset_yr")     

unmatched_coef_tab <- summary(m_unmatched)$coefficients
all_vars_available_to_benchmark <- names(unmatched_coef_tab)

  res <- tibble()
  
  # Sensitivity analysis using benchmark variables and strength of hidden confounders
  for (k_try_curr in k_try) {
    for (vars_names_curr in names(var_benchmark)) {
      vars_use_curr <- intersect(var_benchmark[[vars_names_curr]], names(unmatched_coef_tab))
      
      if (length(vars_use_curr) > 0) {
        try_error <- try(silent = TRUE, expr = {
          bounds_out <- sensemakr:::ovb_bounds.fixest(
            model = m_unmatched,
            treatment = "treat:post",
            benchmark_covariates = list(var_cur = vars_use_curr),
            kd = k_try_curr,
            ky = k_try_curr
          )
          r2yz.dx_use <- bounds_out$r2yz.dx
          r2dz.x_use <- bounds_out$r2dz.x
        })
        
        if (inherits(try_error, "try-error")) {
          add <- tibble(
            proj_id = proj,
            covariate = vars_names_curr,
            k = k_try_curr,
            adjusted_est_offset = -Inf,
            claimed_ate = ate_yr_to_compare,
            adjusted_est_no_offset = -Inf,
            adjusted_ci_low = -Inf,
            adjusted_ci_upp = -Inf
          )
        } else {
          est_curr_reduce <- sensemakr:::adjusted_estimate.fixest(
            model = m_matched,
            treatment = "treat:post",
            r2yz.dx = r2yz.dx_use,
            r2dz.x = r2dz.x_use,
            reduce = TRUE
          )
          est_curr_increase <- sensemakr:::adjusted_estimate.fixest(
            model = m_matched,
            treatment = "treat:post",
            r2yz.dx = r2yz.dx_use,
            r2dz.x = r2dz.x_use,
            reduce = FALSE
          )
          est_curr <- min(c(est_curr_reduce, est_curr_increase))
          se_curr <- sensemakr:::adjusted_se.fixest(
            model = m_matched,
            treatment = "treat:post",
            r2yz.dx = r2yz.dx_use,
            r2dz.x = r2dz.x_use
          )
          
          # adapted as fixest doesn't provide a direct way to call DF
          df_res <- m_matched$nobs - m_matched$nparams
          ci_mult <- qt(p = 0.975, df = df_res)
          
          add <- tibble(
            proj_id = proj,
            covariate = vars_names_curr,
            k = k_try_curr,
            adjusted_est_offset = est_curr,
            claimed_ate = ate_yr_to_compare,
            adjusted_est_no_offset = est_curr + ate_yr_to_compare,
            adjusted_ci_low = adjusted_est_no_offset - ci_mult * se_curr,
            adjusted_ci_upp = adjusted_est_no_offset + ci_mult * se_curr,
            r2yz_dx = r2yz.dx_use,
            r2dz_x = r2dz.x_use
          )
        }
        
        if (k_try_curr == 0) {
          add$covariate <- "none"
          if (vars_names_curr == names(var_benchmark)[1]) {
            res <- res %>% bind_rows(add)
          }
        } else {
          res <- res %>% bind_rows(add)
        }
      }
    }
  }
  
  # Filter and arrange results for plotting
  res_add <- res %>%
    filter(k != 0) %>%
    group_by(k) %>%
    mutate(min_adjusted_ci_low = min(adjusted_ci_low)) %>%
    filter(adjusted_ci_low == min_adjusted_ci_low) %>%
    ungroup() %>%
    arrange(k)
  
  res_add <- res_add %>%
    bind_rows(res %>% filter(k == 0 & covariate == res_add$covariate[1])) %>%
    arrange(k)
  
  res_plot <- res_plot %>% bind_rows(res_add)
  res_plot2 <- res_plot2 %>% bind_rows(res)
}

# Plot settings and preparation for output
min_y <- -8
claimed_ate_thresh <- -7.5
order_proj <- proj_tab %>%
  arrange(-implied_ate) %>%
  pull(proj_id)

res_plot_filt2 <- res_plot2 %>%
  mutate(k = as.character(k)) %>%
  mutate(mn_plot = adjusted_est_no_offset) %>% # ifelse(adjusted_est_no_offset < min_y, min_y, adjusted_est_no_offset)) %>%
  mutate(proj_id = factor(proj_id, levels = order_proj)) %>%
  mutate(Covariate = factor(covariate, levels = var_benchmark_plot_order)) %>%
  mutate(out_of_scale = as.numeric(mn_plot <= min_y)) %>%
  mutate(`Point type` = factor(ifelse(mn_plot <= min_y, paste0("y < ", min_y), "y value plotted"), levels = c("y value plotted", paste0("y < ", min_y))))

# Edit labels
res_plot_filt2 <- res_plot_filt2  %>% 
  mutate(Covariate = factor(Covariate, labels=c("None", "Distance to degradation")))

# -----------------------------------
# Fig 6 - sensitivity plots
# -----------------------------------

df_plot_sensitivity_sub <- res_plot_filt2 %>%
  filter(!proj_id %in% projects_with_low_matched_prop) %>%
  mutate(is_larger = mn_plot < claimed_ate) %>%
  group_by(k, Covariate) %>%
  summarise(n = n(), is_larger = sum(is_larger), .groups = "drop") %>%
  mutate(perc_is_larger = 100 * is_larger / n)

col_palette <- rev(carto_pal(5, "ag_Sunset"))
col_palette[1] <- "#B7B7B7"

# Set factor levels to control order of x-axis
df_plot_sensitivity_sub <- df_plot_sensitivity_sub %>%
  mutate(k_label = factor(paste0(k, "x"), levels = paste0(0:3, "x")))

# Plot 1
plt_1 <- ggplot(
  df_plot_sensitivity_sub,
  aes(x = k_label, y = perc_is_larger, fill = Covariate, color = Covariate)
) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = "single"), width = 0.8) +
  scale_color_manual(values = col_palette, name = "") +
  scale_fill_manual(values = col_palette, name = "") +
  xlab("Strength of hidden confounder\n(x times strength of observed covariate)") +
  ylab("Projects exceeding\nVCS claimed ATE (%)") +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  ) +
  ylim(0, 100)

# Plot 2
pl_out <- ggplot(
  res_plot_filt2 %>%
    filter(!proj_id %in% projects_with_low_matched_prop) %>%
    mutate(k_label = factor(paste0(k, "x"), levels = paste0(0:3, "x"))),
  aes(x = k_label, y = mn_plot, fill = Covariate, color = Covariate)
) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(aes(ymin = adjusted_ci_low, ymax = adjusted_ci_upp), width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(aes(yintercept = claimed_ate), colour = "red", linetype = "dashed") +
  geom_hline(yintercept = 0, colour = "black") +
  scale_color_manual(values = col_palette, name = "") +
  scale_fill_manual(values = col_palette, name = "") +
  xlab("Strength of hidden confounder\n(x times strength of observed covariate)") +
  ylab(expression("Difference in forest loss (% " * yr^{-1} * ")")) +
  theme_bw() +
  facet_wrap(~proj_id, ncol = 5, scales = "free_y") +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    plot.margin = unit(c(0.01, 0.01, 0.01, 0.01), "in"),
    legend.box.spacing = unit(0.001, "in"),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank()
  )

# Combine
gg_out <- plt_1 / pl_out +
  plot_layout(heights = c(0.16, 0.84)) &
  plot_annotation(tag_levels = "a")

ggsave(
  filename = file.path(dir_figures, "sensitivity_analysis_panel_main.png"),
  plot = gg_out,
  width = 12, height = 12, units = "in", dpi = 300
)