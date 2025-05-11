# ------------------------------------------------  
# Run in sequence to reproduce the results
# ------------------------------------------------

source(file.path("code", "01_import_full_data.R"), echo = TRUE)
# OPTIONAL: exploratory matching not needed to reproduce the results. Set to TRUE to run. WARNING: resource intensive!
run_exploratory_matching <- FALSE
if (run_exploratory_matching) {
  source(file.path("code", "02_exploratory_matching.R"), echo = TRUE) 
}
source(file.path("code", "02_propensity_score_subclasses.R"), echo = TRUE)
source(file.path("code", "03_postprocess_design.R"), echo = TRUE)
source(file.path("code", "04_estimate_ATEs.R"), echo = TRUE)
source(file.path("code", "05_plot_ATEs.R"), echo = TRUE)
source(file.path("code", "06_sensitivity_analysis.R"), echo = TRUE)
source(file.path("code", "07_main_figures.R"), echo = TRUE)
