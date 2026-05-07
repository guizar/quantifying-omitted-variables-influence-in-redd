rm(list = ls())
source(file.path("code", "00_preamble.R"), echo = FALSE)

scale_covars <- TRUE            
multi_core   <- TRUE     

n_cores <- if (multi_core) min(5, parallel::detectCores()) else 1
doParallel::registerDoParallel(cores = n_cores)

# Columns of interest
panel_vars_static <- c("mean_slp", "mean_access", "mean_ele") # static covariates from `short`
panel_vars_keys   <- c("country","project","proj_id","gid","year","time_treat","treat")
panel_vars_tv     <- c("arealoss","areaforest","lag_arealoss","lag_areadegraded","dist_degra")

# Covariates to scale (and preserve unscaled)
scale_these <- c("dist_degra","mean_slp","mean_access","mean_ele")

# replace Inf with NA for safe QC filtering
.clean_inf <- function(x) { x[is.infinite(x)] <- NA_real_; x }

# -------------------------------------------------------------------
# Build per-project panel datasets 
# -------------------------------------------------------------------
outl <- foreach::foreach(
  j = 1:nrow(proj_tab),
  .packages = c("tidyverse","dplyr","magrittr","tidyr"),
  .errorhandling = "pass",
  .verbose = TRUE
) %dopar% {
  
  proj_curr  <- proj_tab$proj_id[j]
  out_file   <- file.path(dir_panel_data, paste0("panel_", proj_curr, ".RDS"))
  
  # ---- Read inputs -------------------------------------------------
  long  <- readr::read_csv(file.path(full_data_dir, proj_tab$long_file[j]),  show_col_types = FALSE)
  short <- readr::read_csv(file.path(full_data_dir, proj_tab$summ_file[j]),  show_col_types = FALSE)
  
  # Define post window length: 5 normally, 4 for KHM_PL1748
  n_post <- if (proj_curr == "KHM_PL1748") 4L else 5L
  
  # ---- Project identifiers -----------------------------------------
  long <- long %>%
    mutate(country = proj_tab$country[j],
           project = proj_tab$project[j],
           proj_id = paste(country, project, sep = "_"))
  
  # ---- Join static covariates and select working columns -----------
  panel <- long %>%
    left_join(short %>% select(gid, treat, any_of(panel_vars_static)), by = c("treat","gid")) %>%
    select(all_of(panel_vars_keys), any_of(panel_vars_tv), any_of(panel_vars_static))
  
  # ---- Basic QC &
  panel <- panel %>%
    filter(!is.na(gid), !is.na(year)) %>%                      
    filter(!if_any(all_of(panel_vars_static), is.na)) %>%     
    mutate(across(c(arealoss, areaforest, lag_arealoss, lag_areadegraded, dist_degra),
                  .fns = .clean_inf))                          
  
  # Preserve unscaled copies of covariates
  panel <- panel %>%
    mutate(across(all_of(scale_these), .fns = list(unscl = ~ .), .names = "{.col}_unscl"))
  
  # Standardise covariates
  if (scale_covars) {
    for (v in scale_these) if (v %in% names(panel)) panel[[v]] <- c(scale(panel[[v]]))
  }

  # ---- Construct outcome to match summary estimand -----------------
  # Denominator: per-gid forest area at event time -6 (fixed across years)
  denom <- panel %>%
    filter(time_treat == -6) %>%
    transmute(treat,gid, base_forest = areaforest)
  
  # Annual proportion outcome: z_t = lag_arealoss_t / base_forest
  panel <- panel %>%
    left_join(denom, by = c("treat","gid")) %>%
    mutate(
      base_forest = if_else(is.finite(base_forest) & base_forest > 0, base_forest, NA_real_),
      z = if_else(is.finite(lag_arealoss) & is.finite(base_forest), lag_arealoss / base_forest, NA_real_)
    )
  
  panel <- panel %>%
    filter(time_treat >= -6, time_treat <= n_post) %>%
    mutate(post = as.integer(time_treat >= 1 & time_treat <= n_post))
  
  # ---- Sanity: treatment must be plot-constant ---------------------
  stopifnot(
    all(
      panel %>%
        group_by(proj_id, treat, gid) %>%
        summarise(n_treat = n_distinct(treat), .groups = "drop") %>%
        pull(n_treat) == 1
    )
  )
  
  # ---- Write out per-project panel ---------------------------------
  saveRDS(panel, out_file)
  out_file
}

doParallel::stopImplicitCluster()
gc()
