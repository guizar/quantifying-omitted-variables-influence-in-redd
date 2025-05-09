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
  mutate(implied_ate = 100 * avoided_verra / area_ha) %>%
  arrange(implied_ate)

# Load matching proportions
ordl <- readRDS(file = file.path(dir_output, "ordl.RDS"))
match_prop_tab <- ordl[[dist_use]][[as.character(alpha_use)]]

# # ####
# 
# tmp <- match_prop_tab %>%
#   arrange(prop_matched_alpha_0.00113636363636364) %>%
#   select(proj_id, prop_matched_alpha_0.00113636363636364)
# 
# print(tmp, n = 50)
# # saveRDS(object = tmp, file = "output/tmp.RDS")
# 
# tmp2 <- readRDS(file = "data/output/tmp.RDS")
# 
# 
# ge <- left_join(tmp, tmp2, by = "proj_id")
# 
# pdf("figures/compare.pdf", width = 9, height = 9)
# plot(ge[, 2, drop = T], ge[, 3, drop = T], ty = "p", pch = 19, cex = .7,
#      xlab = "quantifying-omitted-variables-influence-in-redd",
#      ylab = "redd-sens",
#      main = "Comparing proportion treated units matched across code versions")
# abline(0, 1)
# abline(h = .8, v = .8, lty = 2)
# text(labels = ge[, 1, drop = T], ge[, 2, drop = T], ge[, 3, drop = T], cex = .7, pos = 1)
# dev.off()
# 
# 
# # BRA_1094, PER_1067, MDG_1047
# 
# # ######

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
  mean_slp = "mean_slp",
  mean_access = "mean_access",
  mean_ele = "mean_ele"
)
var_benchmark_plot_order <- c("none", "dist_degra", "mean_slp", "mean_ele", "mean_access")

# Loop over each project in the comparison set
for (proj in proj_comp$proj_id) {
  
  # Subset and prepare matched data for analysis
  d_sub <- d %>%
    filter(proj_id == proj, !subclass_remove) %>%
    mutate(difference_yr = 100 * outcome_prop / 5)
  
  ate_yr_to_compare <- proj_tab %>%
    filter(proj_id == proj) %>%
    pull(implied_ate)
  
  d_sub$offset_yr <- ifelse(d_sub$treat == "1", ate_yr_to_compare, 0)
  
  matched_form <- reformulate(response = "difference_yr", termlabels = c("treat", all_explanatory_vars_for_matching, "offset(offset_yr)"))
  lm_matched <- lm(formula = matched_form, data = d_sub)
  summary(lm_matched)
  
  # Load unmatched data to quantify raw associations
  d_qc <- readRDS(file = file.path(dir_qc_data, paste0("qc_data_", proj, ".RDS"))) %>%
    mutate(difference_yr = 100 * outcome_prop / 5)
  
  d_treat <- d_qc %>%
    filter(treat == "1")
  
  # Sample control units to match treated units
  # Set seed for reproducibility
  set.seed(1234)
  d_control <- d_qc %>%
    filter(treat == "0") %>%
    sample_n(size = nrow(d_treat), replace = FALSE)
  
  d_unmatched <- bind_rows(d_treat, d_control)
  d_unmatched$offset_yr <- ifelse(d_unmatched$treat == "1", ate_yr_to_compare, 0)
  
  lm_unmatched <- lm(formula = matched_form, data = d_unmatched)
  unmatched_coef_tab <- summary(lm_unmatched)$coefficients
  all_vars_available_to_benchmark <- rownames(unmatched_coef_tab)
  
  res <- tibble()
  
  # Sensitivity analysis using benchmark variables and strength of hidden confounders
  for (k_try_curr in k_try) {
    for (vars_names_curr in names(var_benchmark)) {
      vars_use_curr <- intersect(var_benchmark[[vars_names_curr]], rownames(unmatched_coef_tab))
      
      if (length(vars_use_curr) > 0) {
        try_error <- try({
          bounds_out <- sensemakr:::ovb_bounds.lm(
            model = lm_unmatched,
            treatment = "treat",
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
            adjusted_est_offset = est_curr,
            claimed_ate = ate_yr_to_compare,
            adjusted_est_no_offset = -Inf,
            adjusted_ci_low = -Inf,
            adjusted_ci_upp = -Inf
          )
        } else {
          est_curr <- sensemakr:::adjusted_estimate.lm(
            model = lm_matched,
            treatment = "treat",
            r2yz.dx = r2yz.dx_use,
            r2dz.x = r2dz.x_use,
            reduce = TRUE
          )
          se_curr <- sensemakr:::adjusted_se.lm(
            model = lm_matched,
            treatment = "treat",
            r2yz.dx = r2yz.dx_use,
            r2dz.x = r2dz.x_use
          )
          
          ci_mult <- qt(p = 0.975, df = lm_matched$df.residual)
          
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

# res_plot_filt <- res_plot %>%
#   # filter(claimed_ate > claimed_ate_thresh) %>%
#   filter(!proj_id %in% projects_with_low_matched_prop) %>%
#   mutate(k = as.character(k)) %>%
#   mutate(mn_plot = ifelse(adjusted_est_no_offset < min_y, min_y, adjusted_est_no_offset)) %>%
#   mutate(adjusted_ci_low = ifelse(adjusted_ci_low < min_y, min_y, adjusted_ci_low)) %>%
#   mutate(proj_id = factor(proj_id, levels = order_proj))

res_plot_filt2 <- res_plot2 %>%
  # filter(claimed_ate > claimed_ate_thresh) %>% # this is where the filtering happens
  mutate(k = as.character(k)) %>%
  mutate(mn_plot = adjusted_est_no_offset) %>% # ifelse(adjusted_est_no_offset < min_y, min_y, adjusted_est_no_offset)) %>%
  # mutate(adjusted_ci_low = ifelse(adjusted_ci_low < min_y, min_y, adjusted_ci_low)) %>%
  # mutate(adjusted_ci_upp = ifelse(adjusted_ci_upp < min_y, min_y, adjusted_ci_upp)) %>%
  mutate(proj_id = factor(proj_id, levels = order_proj)) %>%
  mutate(Covariate = factor(covariate, levels = var_benchmark_plot_order)) %>%
  mutate(out_of_scale = as.numeric(mn_plot <= min_y)) %>%
  mutate(`Point type` = factor(ifelse(mn_plot <= min_y, paste0("y < ", min_y), "y value plotted"), levels = c("y value plotted", paste0("y < ", min_y))))

# Edit labels
res_plot_filt2 <- res_plot_filt2  %>% mutate(Covariate = factor(Covariate, labels=c("None", "Distance to degradation", "Slope", "Elevation", "Accessibility")))

# -----------------------------------
# Fig 6 - sensitivity plots
# --------------------------------

#- Summarise the results
df_plot_sensitivity_sub <- res_plot_filt2 %>% 
filter(!proj_id %in% projects_with_low_matched_prop) %>%
mutate (is_larger =  mn_plot < claimed_ate) %>%
group_by(k,Covariate) %>% summarise(n=n(), is_larger=sum(is_larger)) %>% rowwise() %>%
mutate(perc_is_larger = (is_larger/n)*100) %>% ungroup()

col_palette= rev(carto_pal(5, "ag_Sunset"))
col_palette[1] = "#B7B7B7"

plt_1 <- ggplot(
  df_plot_sensitivity_sub %>% filter(k>0), aes(x = factor(paste0(k, "x")), y = perc_is_larger, fill = Covariate, color = Covariate)) +
  geom_bar(stat = 'identity', position = position_dodge2(width = 0.9, preserve = "single"), width = 0.8) +
  scale_shape_manual(values = c(21, 25)) +
  xlab("Strength of hidden confounder\n(x times strength of observed covariate)") +
  ylab("Projects exceeding\nVCS claimed ATE (%)") +
  theme_bw() +
  theme(strip.background = element_blank(), strip.text = element_text(face = "bold")) +
  scale_color_manual(values = col_palette[-1], name = "")  + 
  scale_fill_manual(values = col_palette[-1], name = "") + 
  theme(legend.position="none")

# file_curr <- file.path(dir_figures) 
# ggsave(file_curr,plt_1, width=3, height=3, units='in', dpi=300)

# Produce panel ggplot
pl_out <- ggplot(res_plot_filt2 %>% filter(!proj_id %in% projects_with_low_matched_prop), aes(x = factor(paste0(k, "x")), y = mn_plot, fill = Covariate, color = Covariate)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  scale_shape_manual(values = c(21, 25)) +
  geom_errorbar(aes(ymin = adjusted_ci_low, ymax = adjusted_ci_upp), width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(aes(yintercept = claimed_ate), colour = "red", linetype = "dashed") +
  geom_hline(aes(yintercept = 0), colour = 1) +
  xlab("Strength of hidden confounder\n(x times strength of observed covariate)") +
  ylab("Difference in forest loss (%"~yr^-1*')') +
  theme_bw() +
  theme(strip.background = element_blank(), strip.text = element_text(face = "bold")) +
  facet_wrap(~ proj_id, ncol=5, scales='free_y') +
  # ylim(min_y, 1) +
  scale_color_manual(values=col_palette, name="")  + scale_fill_manual(values=col_palette, name="") +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5), 
    legend.position = "bottom",
    plot.margin = unit(c(0.01,0.01,0.01,0.01), "in"),
          # legend.margin=margin(0.1,0.1,0.1,0.1),
          legend.box.spacing = unit(0.001, "in"),
          axis.text.x.top = element_blank(),  # Remove top x-axis text
          axis.ticks.x.top = element_blank()
  )

# -- combine summary and panels
library(patchwork)
file_curr <- file.path(dir_figures, "sensitivity_analysis_main.png") 
gg_out <- plt_1 / pl_out  +  plot_layout(
    # guides = "collect", 
    heights = c(0.16,0.84 ),
    ncol = 1,  # Ensure layout is vertical
    nrow = 2   # Specify number of rows
  ) & 
  plot_annotation(tag_levels = 'a') # Add tags to panels

ggsave(file_curr,gg_out, width=12, height=12, units='in', dpi=300)


# -----------------------------------
# Suppl. - sensitivity plots
# --------------------------------

#- Summarise the results
df_plot_sensitivity <- res_plot_filt2 %>% mutate (is_larger =  mn_plot < claimed_ate) %>%
group_by(k,Covariate) %>% summarise(n=n(), is_larger=sum(is_larger)) %>% rowwise() %>%
mutate(perc_is_larger = (is_larger/n)*100) %>% ungroup()

col_palette= rev(carto_pal(5, "ag_Sunset"))
col_palette[1] = "#B7B7B7"
# palette_check(col_palette, plot = TRUE)

plt_1 <- ggplot(
  df_plot_sensitivity %>% filter(k>0), aes(x = factor(paste0(k, "x")), y = perc_is_larger, fill = Covariate, color = Covariate)) +
  geom_bar(stat = 'identity', position = position_dodge2(width = 0.9), width = 0.8) +
  # scale_shape_manual(values = c(21, 25)) +
  xlab("Strength of hidden confounder\n(x times strength of observed covariate)") +
  ylab("Projects exceeding\nVCS claimed ATE (%)") +
  theme_bw() +
  theme(strip.background = element_blank(), strip.text = element_text(face = "bold")) +
  scale_color_manual(values = col_palette[-1], name = "")  + 
  scale_fill_manual(values = col_palette[-1], name = "") + 
  # scale_y_continuous(limits = c(0, 15))  +
  theme(legend.position="none")

# file_curr <- file.path(dir_figures) 
# ggsave(file_curr,plt_1, width=3, height=3, units='in', dpi=300)

# Produce panel ggplot
pl_out <- ggplot(res_plot_filt2, aes(x = factor(paste0(k, "x")), y = mn_plot, fill = Covariate, color = Covariate,)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  scale_shape_manual(values = c(21, 25)) +
  geom_errorbar(aes(ymin = adjusted_ci_low, ymax = adjusted_ci_upp), width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(aes(yintercept = claimed_ate), colour = "red", linetype = "dashed") +
  geom_hline(aes(yintercept = 0), colour = 1) +
  xlab("Strength of hidden confounder\n(x times strength of observed covariate)") +
  ylab("Difference in forest loss (%"~yr^-1*')') +
  theme_bw() +
  theme(strip.background = element_blank(), strip.text = element_text(face = "bold")) +
  facet_wrap(~ proj_id, ncol=5, scales='free_y') +
  # ylim(min_y, 1) +
  scale_color_manual(values=col_palette, name="")  + scale_fill_manual(values=col_palette, name="")  +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5), 
    legend.position = "bottom",
    plot.margin = unit(c(0.01,0.01,0.01,0.01), "in"),
          # legend.margin=margin(0.1,0.1,0.1,0.1),
          legend.box.spacing = unit(0.001, "in"),
          axis.text.x.top = element_blank(),  # Remove top x-axis text
          axis.ticks.x.top = element_blank()
  )

# -- combine summary and panels
library(patchwork)
file_curr <- file.path(dir_figures, "sensitivity_analysis_SI.png") 
gg_out <- plt_1 / pl_out  +  plot_layout(
    # guides = "collect", 
    heights = c(0.16,0.84 ),
    ncol = 1,  # Ensure layout is vertical
    nrow = 2   # Specify number of rows
  ) & 
  plot_annotation(tag_levels = 'a')

ggsave(file_curr,gg_out, width=12, height=12, units='in', dpi=300)


# ------------------------------------------------
# R2 plots
# ------------------------------------------------
recode_map <- setNames(
  c("1:FALSE", "1:TRUE"),
  c(inclusion_lab[5], inclusion_lab[4])
)

df_r2_plot <- res_plot_filt2 %>% 
  filter(Covariate != 'None') %>% 
  filter(k == 1) %>% 
  mutate(
    projects_with_low_matched_prop = proj_id %in% projects_with_low_matched_prop, 
    strength_col = fct_cross(as.factor(k), as.factor(projects_with_low_matched_prop)),
    strength_col = fct_recode(strength_col, !!!recode_map)
  )

# define cols
# col_darks <- rev(generate_palette('black', modification = "go_lighter", n_colours = 3, view_palette = F))
# col_greens <- rev(generate_palette(axis_text_color_dimmed, modification = "go_lighter", n_colours = 3, view_palette = F))
scatter_cols <- c(map_cols[5],map_cols[4])

# plot
pl_out <- ggplot(df_r2_plot, aes(x = r2yz_dx, y = r2dz_x, colour = strength_col)) +
  geom_point() +
  scale_colour_manual(values = scatter_cols, name = "") +
  theme_minimal() +
  labs(colour = "Strength of Confounder") + 
  xlab(expression(R[ y %~% z * "|" * d * "," * bold(x) ]^2)) +
  ylab(expression(R[ d %~% z * "|" * bold(x) ]^2)) +
  facet_wrap(.~Covariate, ncol=2, nrow=2) +
  xlim(0,1) + ylim(0,1) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5), 
    legend.position = "bottom",
    legend.box = "horizontal", 
    legend.box.just = "center", 
    legend.spacing.y = unit(0.2, "cm"), 
    strip.background = element_blank(), 
    strip.text = element_text(face = "bold"),
    axis.text.x.top = element_blank(),  
    axis.ticks.x.top = element_blank()
  ) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE)) 

# Save the plot
ggsave(
  filename = file.path(dir_figures, "sensitivity_analysis_r2_plot.png"),
  plot = pl_out,
  width = 6,
  height = 6,
  dpi = 300
)



# -------------------------------------------------
# Produce individual plots, stored in a list object for patchwork
# ---------------------------------------------------

# plots_ls <- list()

# plt_1 <- ggplot(df_plot_sensitivity, aes(x = factor(paste0(k, "x")), y = perc_is_larger, fill = Covariate, color = Covariate)) +
#   geom_point(position = position_dodge(width = 0.2), size = 1) +
#   scale_shape_manual(values = c(21, 25)) +
#   labs(x = "Strength of hidden confounder (as multiple of strength of observed covariate)",
#        y = "Percent of projects\nexceeding VCS claimed ATT") +
#   theme_bw() +
#   theme(strip.background = element_blank(), strip.text = element_text(face = "bold"), legend.position='none') +
#   scale_color_manual(values=col_palette, name="")  + scale_fill_manual(values=col_palette, name="")

# plots_ls[['summary']] <- plt_1

# for (s in unique(as.character(res_plot_filt2$proj_id))) {
# pl_out <- ggplot(res_plot_filt2 %>% filter(proj_id==s), aes(x = factor(paste0(k, "x")), y = mn_plot, fill = Covariate, color = Covariate, shape = `Point type`)) +
#   geom_point(position = position_dodge(width = 0.5), size = 1) +
#   scale_shape_manual(values = c(21, 25)) +
#   geom_errorbar(aes(ymin = adjusted_ci_low, ymax = adjusted_ci_upp), width = 0, position = position_dodge(width = 0.5)) +
#   geom_hline(aes(yintercept = claimed_ate), colour = "red", linetype = "dashed") +
#   geom_hline(aes(yintercept = 0), colour = 1) +
#   labs(x = "Strength of hidden confounder (as multiple of strength of observed covariate)",
#        y = "Difference in deforestation rate (% / year)") +
#   theme_bw() +
#   theme(strip.background = element_blank(), strip.text = element_text(face = "bold")) +
#   ylim(min_y, 1) +
#   scale_color_manual(values=col_palette, name="")  + scale_fill_manual(values=col_palette, name="")

# plots_ls[[s]] <- pl_out 
# }
