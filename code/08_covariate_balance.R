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
matching_out <- readRDS(file = file.path(match_out_dir, file_name))
sm <- summary(matching_out)
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

## --------------------------------------------------------------------- 
# Quantifying covariate overlap in projects with projects_with_low_matched_prop
## --------------------------------------------------------------------- 

# Define t-tests fun
perform_t_test  <- function(data, cov_col = "covariate", value_col = "value", group_col = "is_matched") {
  # Get unique covariates
  covs <- unique(data[[cov_col]])
  
  # Perform t-tests for each covariate
  results <- map_dfr(covs, function(cov) {
    # Filter data for this covariate
    cov_data <- data %>% 
      filter(!!sym(cov_col) == cov)
    
    # Get values for matched and unmatched groups
    matched_vals <- cov_data %>% 
      filter(!!sym(group_col) == TRUE) %>% 
      pull(!!sym(value_col))
    
    unmatched_vals <- cov_data %>% 
      filter(!!sym(group_col) == FALSE) %>% 
      pull(!!sym(value_col))
    
    # Perform t-test with error handling
    tryCatch({
      t_result <- t.test(matched_vals, unmatched_vals, paired = FALSE)
      
      # Format results with broom and add covariate name
      tidy_result <- tidy(t_result) %>%
        mutate(proj_id = unique(data$proj_id),
               covariate = cov,
               is_constant = FALSE)
      
      return(tidy_result)
    }, error = function(e) {
      # Check if error is due to constant data
      if(grepl("constant", e$message)) {
        # Create a custom result row for constant data
        return(tibble(
          proj_id = unique(data$proj_id),
          covariate = cov,
          estimate = mean(matched_vals, na.rm = TRUE), # Or NA if preferred
          estimate1 = mean(matched_vals, na.rm = TRUE),
          estimate2 = mean(unmatched_vals, na.rm = TRUE),
          statistic = NA_real_,
          p.value = NA_real_,
          parameter = NA_real_,
          conf.low = NA_real_,
          conf.high = NA_real_,
          method = "Data constant - t-test skipped",
          alternative = NA_character_,
          is_constant = TRUE
        ))
      } else {
        # For other errors, return the error message
        return(tibble(
          proj_id = unique(data$proj_id),
          covariate = cov,
          method = paste("Error:", e$message),
          is_constant = NA,
          estimate = NA_real_,
          estimate1 = NA_real_,
          estimate2 = NA_real_,
          statistic = NA_real_,
          p.value = NA_real_,
          parameter = NA_real_,
          conf.low = NA_real_,
          conf.high = NA_real_,
          alternative = NA_character_
        ))
      }
    })
  })
  
  return(results)
}

## ----------------------------------------------------------------------------- 
#  Examine covariate overlap: by plotting density curves and computing t-tests
# ' The followng chunk iterates by proj_id, pulls the relevant matched/unmateched
# ' observations, plots density curves for examined covariates and 
# ' calculates t-tests between matched and unmatched covariate values
## ----------------------------------------------------------------------------- 

# Define target table for t-tests
d_tests <-  data.frame()

# pull covars from ps_score_form
cov_terms <- attr(x = terms(ps_score_form), which = "term.labels")

for (proj_id in projects_with_low_matched_prop) {
  
  j <- match_prop_tab$id[match_prop_tab$proj_id == proj_id]
  perc_matched <- match_prop_tab$perc_matched[match_prop_tab$proj_id == proj_id]
  
  caliper_val <- do_tab$caliper_val[j]  # Caliper value for matching
  dist_use <- do_tab$dist_use[j]  # Distance measure for matching 
  match_w_replacement <- do_tab$match_w_replacement[j] # repl
  
  # load matched dataset
  # proj_id = 'SLE_1201'; caliper_val = 0.2; dist_use = 'mahalanobis'; match_w_replacement = TRUE # <- TEST ONLY
  file_name <- paste0("design_proj_", proj_id, "_caliper_", caliper_val, "_dist_", dist_use, "_replace_", match_w_replacement, ".RDS")
  
  if (!file.exists(file.path(d_matched_dir, file_name))) {
    message("RDS file does not exist:", file_name)
    next  # Move to the next iteration of the loop
  } else {
    
    # -----------------------
    # 1. read matched dataset
    # -----------------------

    d_matched_out <- readRDS(file = file.path(d_matched_dir, file_name))
    matched_gids <- d_matched_out$d_matched %>% filter(treat == 1) %>% .$gid
    
    d_qc <- readRDS(file = file.path(dir_qc_data, paste0("qc_data_", proj_id, ".RDS")))
    # 'reinforce' treat as a binary variable
    d_qc$treat <- as.integer(d_qc$treat)
    # filter treated only AND flag if matched
    d_qc <-  d_qc %>% filter(treat == 1) %>% mutate(is_matched = gid %in% matched_gids)
    
    # -----------------------
    # 2. plot density curves
    # -----------------------

    file_curr <- file.path(dir_figures, paste0("cov_balance_treated_", proj_id, ".png")) 

    ggp = d_qc %>% 
      select(all_of(c('is_matched', cov_terms))) %>% 
      gather(var, val, -is_matched) %>%
      ggplot(aes(val, fill = is_matched, color = is_matched)) +
      geom_density(alpha = 0.5) + 
      facet_wrap(.~var, scales='free') + 
      ggtitle(proj_id, paste0("Percent matched: ",perc_matched,"%")) +
      theme_bw() +
      theme(strip.background = element_blank(), 
            strip.text = element_text(face = "bold"), 
            legend.position = 'bottom')  
    
    # ggsave(file_curr, ggp, width = 10, height = 10, units = 'in', dpi = 300)

    # -----------------------
    # 3. Perform t-tests
    # -----------------------

    d_qc_long <- d_qc %>% ungroup() %>% 
    select(-c(outcome, total_loss, outcome_prop, total_prop, x, y, id_ctrl, project, area_forest_t_1990_unscl, area_forest_t_minus_6_unscl, area_forest_t_zero_unscl, area_loss_t_minus_6_unscl, area_loss_t_zero_unscl, area_loss_t_plus_5_unscl)) %>% 
    gather(covariate,value, -c(country,proj_id,gid,is_matched)) %>%
    filter(covariate %in% cov_terms)

    d_test_proj <- perform_t_test(d_qc_long) %>% mutate(perc_matched=perc_matched)
    d_tests <- bind_rows(d_tests,d_test_proj)

  }
}

# save to disk
file_curr <- file.path(dir_tables, "treated_covariate_tests.csv") 
write_csv(d_tests ,file_curr)

# ---------
# SUMMARISE
# ---------

file_curr <- file.path(dir_tables, "treated_covariate_tests.csv") 
d_tests <- read_csv(file_curr)

covars_order <- c(
'Deforestation\n long-term' = 'pre_long_term_trend',
'Deforestation\n short-term' = 'pre_short_term_trend',
'Area forest\n(baseline)' = 'area_forest_t_minus_6',
"Distance to rec.\ndegraded areas" = 'dist_degra',
"Slope" = 'mean_slp',
"Elevation" = 'mean_ele',
"Accessibility" = 'mean_access'
)

# make significance
d_tests_sum <- d_tests %>% 
  filter(!covariate %in% c('pre_short_term_trend_prop', 'pre_long_term_trend_prop')) %>%
  filter(proj_id !='IDN_1477') %>%
  mutate(significance = case_when( p.value < 0.001 ~ "***", p.value < 0.01 ~ "**", p.value < 0.05 ~ "*", TRUE ~ ".")) %>% 
  mutate(
    covariate = factor(covariate, levels=covars_order, labels=names(covars_order)),
    significance = factor(significance,levels = c('.','*','**','***')),
    y_lab = paste0(proj_id," (",round(perc_matched,2),"%)")
  )

# ------------------------
# Plot a simple sig table
# ------------------------

ggp <- ggplot(d_tests_sum, aes(covariate, y_lab, fill = significance)) + 
  geom_tile(color='black', size=0.5) +
  geom_text(aes(label = round(estimate, 2)), size = 3) +
  scale_fill_manual(values = c(
    "none" = "#F5F5F7",  "*" = "#FE9797", "**" = "#DD5B82",  "***" = "#913175"
  )) +
  theme_bw() +  xlab("") + ylab("") +
  theme(panel.border = element_rect( fill=NA, colour = "black", size=1),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        legend.title=element_blank(),
        axis.line = element_blank(), 
        legend.position = "bottom", 
        axis.title.x = element_text(margin = ggplot2::margin(t = 1)),
        axis.title.y = element_text(margin = ggplot2::margin(r = 1)),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "in"),
        legend.margin=ggplot2::margin(0.1,0.1,0.1,0.1),
        legend.box.spacing = unit(0.001, "in"),
        )
  

file_curr <- file.path(dir_figures, "covariate_differences.png") 
ggsave(file_curr,ggp, width=8, height=6, units='in', dpi=300)

# ------------------------
# produce summary table
# -----------------------

# PREPARE TABLE
out_table <-  d_tests_sum %>% mutate(
  perc_matched  = round(perc_matched,2)
) %>% select(proj_id,y_lab,covariate,estimate, estimate1, estimate2, statistic,  p.value, parameter, conf.low, conf.high) %>%
arrange(proj_id,covariate) %>% select(-proj_id)

cls = c("Project ID","Covariate","Mean. diff.","Matched mean","Unmatched mean", "T", "p-value","DF","CI (upper)","CI (lower)")
colnames(out_table) = cls

# Render table to MD
md_table <- capture.output(pandoc.table(out_table, style='multiline',split.table = Inf, digits=3))
writeLines(md_table, file.path("tables", "covariate_diff_summaries.md"))

# Render MD to Docx (requires Pandoc)
# system(paste0("pandoc ", file.path("tables", "covariate_diff_summaries.md"), " -o ", file.path("tables", "SuppTable_2.docx")))
