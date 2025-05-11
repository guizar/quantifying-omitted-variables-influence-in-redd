library(MatchIt)

# setwd(file.path('figures')) # FOR TESTING

# Load required preamble
source(file.path("code", "00_preamble.R"), echo = TRUE)
# --- For consistency we will build for the objects constructed int he previous exploratory plots
source(file.path("code", "05_plot_ATEs.R"), echo = TRUE)
source(file.path("code", "06_sensitivity_analysis.R"), echo = TRUE)

# change match_prop_tab  to %
match_prop_tab <- match_prop_tab %>% mutate(perc_matched = eval(parse(text = paste0('prop_matched_alpha_', as.character(alpha_use)))) * 100)

# define recipient table
df_covariate_info <- data.frame()

for (j in match_prop_tab$id) {

proj_id <- do_tab$proj_id[j]  # Current project ID
caliper_val <- do_tab$caliper_val[j]  # Caliper value for matching
dist_use <- do_tab$dist_use[j]  # Distance measure for matching 
match_w_replacement <- do_tab$match_w_replacement[j] # repl

# proj_id = 'COG_1052'; caliper_val = 0.1; dist_use = 'randomforest'; match_w_replacement = TRUE # <- TEST ONLY
file_name <- paste0("design_proj_", proj_id, "_caliper_", caliper_val, "_dist_", dist_use, "_replace_", match_w_replacement, ".RDS")

if (!file.exists(file.path(match_out_dir, file_name))) {
    message("RDS file does not exist:", file_name)
    next  # Move to the next iteration of the loop
  } else {

# read matching diagnostics
# matching_out <- readRDS(file = file.path(match_out_dir, file_name))
sm <- readRDS(file = file.path(match_out_dir, file_name))
# sm <- summary(matching_out)
sum.matched= as_tibble(sm[['sum.matched']])
sum.matched$variable= row.names(sm[['sum.matched']])
sum.matched = sum.matched %>%
  rename(std_mean_diff = `Std. Mean Diff.`) %>%
  mutate(proj_id = proj_id)

df_covariate_info <- bind_rows(df_covariate_info,sum.matched)
}
}

## --- Plot covariate balance summaries (all projects)

# PALETTE
pal_covars= carto_pal(10, "Vivid")

# PLOT
ggp_out <- df_covariate_info %>%
  mutate(matched_covar = abs(std_mean_diff) < 0.25) %>%
  filter(variable != "distance") %>%
  ggplot(.,aes(std_mean_diff,proj_id,
    shape=matched_covar, group=proj_id, color=variable)) +
  geom_point() +
  xlab("Std. Mean Diff.") + ylab("site") +
  xlim(-1,1) +
  theme_bw() +
  scale_color_manual(values=pal_covars, na.translate = F) +
  geom_vline(xintercept=0.25, lty=3, color='red') +
  geom_vline(xintercept=-0.25, lty=3, color='red') +
  
  theme(axis.text.y = element_text(color = ifelse(levels(factor(df_covariate_info$proj_id)) %in% projects_with_low_matched_prop, 
                                                 axis_text_color_dimmed, 
                                                 ifelse(levels(factor(df_covariate_info$proj_id)) %in% not_matched, 
                                                        "grey", 
                                                        "black"))))

file_curr <- file.path(dir_figures, "covariate_balance_si.png") 
ggsave(file_curr,ggp_out, width=8, height=8, units='in', dpi=300)
