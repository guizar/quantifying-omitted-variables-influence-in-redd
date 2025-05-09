# Load preamble settings and functions
source(file.path("code", "00_preamble.R"), echo = TRUE)

# Load previously saved model fits and matched data
fit_spatial <- TRUE  # Control flag for whether spatial models were fitted
outl <- readRDS(file = file.path(dir_output, paste0("model_fits_spatial_", fit_spatial, ".RDS")))
d <- readRDS(file = file.path(dir_output, paste0("all_dat_matched_alpha_", alpha_use, ".RDS")))

# Load the ordered list of matching information
ordl <- readRDS(file = file.path(dir_output, "ordl.RDS"))

# Extract quality control data for the specified alpha
qc_tab <- ordl[[as.character(alpha_use)]]
qc_tab$prop_matched <- qc_tab[[paste0("prop_matched_alpha_", alpha_use)]]  # Proportion matched for current alpha
qc_tab$insufficient_matched <- qc_tab$prop_matched < prop_matched_thresh  # Identify projects with insufficient matches
prob_projects <- qc_tab$proj_id[qc_tab$insufficient_matched]  # List of problematic projects

# Load combined results for different models
combined_lm_results_simple <- readRDS(file = file.path(dir_output, "combined_lm_results_simple.RDS"))
combined_lm_results_simple_adjusted <- readRDS(file = file.path(dir_output, "combined_lm_results_simple_adjusted.RDS"))
combined_lm_results <- readRDS(file = file.path(dir_output, "combined_lm_results.RDS"))
combined_lm_adj_results <- readRDS(file = file.path(dir_output, "combined_lm_adj_results.RDS"))
combined_lm_ps_weights_results <- readRDS(file = file.path(dir_output, "combined_lm_ps_weights_results.RDS"))
combined_cat_quant_results <- readRDS(file = file.path(dir_output, "combined_cat_quant_results.RDS"))
combined_lme_adj_results <- readRDS(file = file.path(dir_output, "combined_lme_adj_results.RDS"))

# Prepare data for plotting: categorical/quantitative model results
plot_df <- combined_cat_quant_results %>%
  select(proj_id, ate, ate_se) %>%
  mutate(method = "Cat/quant", 
         ate_yr = -ate * 100 / 5,  # Convert to annual rate of deforestation (in %)
         ate_yr_se = ate_se * 100 / 5)  # Convert standard error to % per year

# Label for the y-axis
ylab_curr <- "Difference in deforestation rate (% / year)"

# Method names for each type of model
methnames <- list(
  lm_subclass = "LM in PS subclasses",
  doubly_robust = "Doubly robust LM in\nPS subclasses",
  cat_quant = "Categorical/quantitative model in\nPS subclasses",
  spatial = "Spatially autocorrelated LM",
  lm_simple = "Unadjusted LM on matched data",
  lm_simple_adjusted = "Adjusted LM on matched data",
  lm_weight = "PS weighted LM on matched data"
)

# Order in which methods will be plotted
order_methods <- c("lm_subclass", "cat_quant", "spatial", "lm_simple", "lm_simple_adjusted", "lm_weight", "doubly_robust")

# Combine results from different models into a single data frame for plotting
comp_meths <- bind_rows(
  combined_lm_results_simple %>%
    mutate(method = methnames$lm_simple, 
           ate_yr = ate * 100 / 5,  # Convert to annual rate of deforestation
           ate_yr_se = ate_se * 100 / 5) %>%
    select(proj_id, method, ate_yr, ate_yr_se),
  
  combined_lm_results_simple_adjusted %>%
    mutate(method = methnames$lm_simple_adjusted, 
           ate_yr = ate * 100 / 5, 
           ate_yr_se = ate_se * 100 / 5) %>%
    select(proj_id, method, ate_yr, ate_yr_se),
  
  combined_lm_results %>%
    mutate(method = methnames$lm_subclass, 
           ate_yr = ate * 100 / 5, 
           ate_yr_se = ate_se * 100 / 5) %>%
    select(proj_id, method, ate_yr, ate_yr_se),
  
  combined_lm_adj_results %>%
    mutate(method = methnames$doubly_robust, 
           ate_yr = ate * 100 / 5, 
           ate_yr_se = ate_se * 100 / 5) %>%
    select(proj_id, method, ate_yr, ate_yr_se),
  
  combined_lm_ps_weights_results %>%
    mutate(method = methnames$lm_weight, 
           ate_yr = ate * 100 / 5, 
           ate_yr_se = ate_se * 100 / 5) %>%
    select(proj_id, method, ate_yr, ate_yr_se),
  
  combined_cat_quant_results %>%
    mutate(method = methnames$cat_quant, 
           ate_yr = ate * 100 / 5, 
           ate_yr_se = ate_se * 100 / 5) %>%
    select(proj_id, method, ate_yr, ate_yr_se)
)

# Add spatial results if applicable
if (fit_spatial) {
  comp_meths %<>%
    bind_rows(combined_lme_adj_results %>%
                mutate(method = methnames$spatial, 
                       ate_yr = ate * 100 / 5, 
                       ate_yr_se = ate_se * 100 / 5) %>%
                select(proj_id, method, ate_yr, ate_yr_se))
}

# Order projects by ATE (from categorical/quantitative model)
order_vcs <- plot_df %>%
  arrange(-ate_yr) %>%
  pull(proj_id)

# Prepare data for plotting with ordered project IDs and method labels
comp_meths %<>%
  mutate(proj_id = factor(proj_id, levels = order_vcs)) %>%  # Set project ID order
  mutate(method = factor(method, levels = methnames[order_methods]))  # Set method order

match_prop_tab <- ordl[[dist_use]][[as.character(alpha_use)]]

projects_with_low_matched_prop <- match_prop_tab %>%
  filter(match_prop_tab[, paste0("prop_matched_alpha_", alpha_use), drop = T] < prop_matched_thresh & match_prop_tab[, paste0("prop_matched_alpha_", alpha_use), drop = T] >0) %>%
  pull(proj_id)

not_matched <- match_prop_tab %>%
  filter( match_prop_tab[, paste0("prop_matched_alpha_", alpha_use), drop = T] ==0) %>%
  pull(proj_id)



# Set dodge width for separating error bars
dodge_width <- 0.75

# Save the plot as PNG
filename_out <- "ATE_by_method.png"
file_meth_strat <- file.path("figures", filename_out)

# Create a column in comp_meths to mark projects with low matched proportion
comp_meths <- comp_meths %>%
  mutate(low_matched = ifelse(proj_id %in% projects_with_low_matched_prop, TRUE, FALSE))

# Order projects based on ATE (from categorical/quantitative model)
order_vcs <- plot_df %>%
  arrange(-ate_yr) %>%
  pull(proj_id)

# Define text colour for projs <80% matched
axis_text_color_dimmed <- '#99C945'

# Create the plot: ATE estimates by method across projects
library(ggplot2)
plot_out <- ggplot(comp_meths, aes(x = proj_id, y = ate_yr, color = method)) +
  geom_hline(lwd = 0.5, yintercept = 0) +  # Horizontal line at y=0
  geom_errorbar(aes(ymin = ate_yr - 2 * ate_yr_se, ymax = ate_yr + 2 * ate_yr_se), width = 0.2, position = position_dodge(width = dodge_width)) +  
  geom_point(position = position_dodge(width = dodge_width)) +  # Points for ATE
  labs(title = "", 
       x = "", 
       y = ylab_curr) +  # Labels
  theme_bw() +  # Minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        legend.title = element_text(size = 12), 
        legend.position = "right", 
        axis.title.x = element_text(margin = ggplot2::margin(t = 20)),
        axis.title.y = element_text(margin = ggplot2::margin(r = 20))) +  # Adjust text and legend
  
  # Custom coloring for the x-axis labels
  scale_x_discrete(limits = order_vcs,  # Maintain project order on x-axis
                   labels = function(proj_id) {
                     ifelse(proj_id %in% projects_with_low_matched_prop, 
                            paste0(proj_id), proj_id)  # Label with project ID
                   }) +
  
  scale_color_brewer(palette = "Set1", name = "Method") +

  theme_bw() + 
  theme(panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "in"),
        legend.title=element_blank(),
        # axis.text.y = element_text(margin = margin(r = 1)),
        axis.line = element_blank(), 
        legend.position = "bottom",
        legend.box = "vertical",
        legend.direction = "vertical",
        legend.spacing.x = unit(0.2, "cm"),
        axis.title.x = element_text(margin = ggplot2::margin(t = 1)),
        axis.title.y = element_text(margin = ggplot2::margin(r = 1)),
        axis.text.x.top = element_blank(),  # Remove top x-axis text
        axis.ticks.x.top = element_blank(),
        axis.text.x = element_text(
            color = ifelse(order_vcs %in% projects_with_low_matched_prop, axis_text_color_dimmed, "black"),
            angle = 90, hjust = 1
            )
        )

# export ggplot
ggsave(file_meth_strat,plot_out, width=8, height=11, units='in', dpi=300)
saveRDS(comp_meths, file = file.path(dir_analysis_outputs, "comp_meths.RDS"))

