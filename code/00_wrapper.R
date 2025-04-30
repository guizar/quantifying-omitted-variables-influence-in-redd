# ------------------------------------
# 1. Declare GitHub folder location
# -------------------------------------

# Declare path to project folder from either HOME (Unix) or USERPROFILE (Windows) where the GitHub folder is located

PATH_FROM_HOME <- c("vlab")  # should follow the file.path() convention, where folders are provided in a character vector format; e.g. c('folder','subfolder'). Leave empty if the GitHub folder is located directly inside your (HOME/USERPROFILE) folder

SERVER_PATH <- NA # Defaults to PATH_FROM_HOME. Don't change unless you're processing the data in a diferent directory

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

  # Build base directory consistently across platforms
  base_dir <- file.path(home_dir, PATH_FROM_HOME, project_folder)
  
  # Set data directory (same as base by default)
  server_dir <- base_dir
  
  # adjust data path (server processing)
  if (!is.na(SERVER_PATH)) {
    server_dir <- file.path(SERVER_PATH,project_folder)
  }
  
  # return lists
  list(
    PATH = base_dir,
    SERVER_PATH = if (is.na(SERVER_PATH)) NA else server_dir
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