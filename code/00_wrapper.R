# ------------------------------------
# 1. Declare GitHub folder location
# -------------------------------------

# Declare path to project folder from either HOME (Unix) or USERPROFILE (Windows) where the GitHub folder is located

PATH_FROM_HOME <- c("vlab")  # Should follow the `file.path()` convention, where folders are provided in a character vector format, e.g., `c('folder', 'subfolder')`. Leave empty if the GitHub folder is located directly inside your `(HOME/USERPROFILE)` folder.

SERVER_PATH <- NA # Defaults to `PATH_FROM_HOME`. This effectively defines an alternative path for the project folder. It's set up for situations where the raw data is located in a different directory other than `PATH_FROM_HOME`. For most cases this should be left as NA

# ---------------------------------------
# 2. Declare consistent dir paths funtions
# --------------------------------------

setup_paths <- function() {
  
  project_folder <- "quantifying-omitted-variables-influence-in-redd"

  # Get base home directory based on platform
  home_dir <- if (.Platform$OS.type == "windows") {
    Sys.getenv('USERPROFILE')
  } else {
    Sys.getenv('HOME')
  }

  # Define GITHUB folder
  if (length(PATH_FROM_HOME) == 1) {
    PATH <- file.path(home_dir,PATH_FROM_HOME,project_folder)} else {
    PATH <- file.path(home_dir,
        do.call(file.path,as.list(PATH_FROM_HOME)),
        project_folder)
}
  
  # adjust data path (server processing)
  if (!any(is.na(SERVER_PATH)))  {
  SERVER_PATH <- file.path(
    do.call(file.path,as.list(SERVER_PATH)),
    project_folder)} else {
    SERVER_PATH <- PATH
    }
  
  # return lists
  list(
    PATH = normalizePath(PATH),
    SERVER_PATH = normalizePath(SERVER_PATH)
  )
}

# ------------------------  
# 3. Declare project paths
# ------------------------  

paths <- setup_paths()
pth <- paths$PATH
pth_server <- paths$SERVER_PATH


# ------------------------------------------------  
# Run in sequence to reproduce the results
# ------------------------------------------------

source(file.path(pth, "code", "01_import_full_data.R"), echo=TRUE)
source(file.path(pth, "code", "02_exploratory_matching.R"), echo=TRUE) # OPTIONAL, not needed to reproduce the results.  WARNING: resource intensive!
source(file.path(pth, "code", "02_propensity_score_subclasses.R"), echo=TRUE)
source(file.path(pth, "code", "03_postprocess_design.R"), echo=TRUE)
source(file.path(pth, "code", "04_estimate_ATEs.R"), echo=TRUE)
source(file.path(pth, "code", "05_plot_ATEs.R"), echo=TRUE)
source(file.path(pth, "code", "06_sensitivity_analysis.R"), echo=TRUE)
source(file.path(pth, "code", "07_main_figures.R"), echo=TRUE)
source(file.path(pth, "code", "08_covariate_balance.R"), echo=TRUE)