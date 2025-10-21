
panel_data_fit_one_project <- function(dat_i,
                            y = "z",
                            xvars = xvars,
                            fixefs,
                            cluster = c("gid")) {
  
  rhs <- c("treat:post", xvars)
  # Fixed effects: plot (gid) and relative time (time_treat)
  fml <- as.formula(paste0(y, " ~ ", paste(rhs, collapse = " + "), " | ", paste(fixefs, collapse = " + ")))
  m <- fixest::feols(
    fml,
    data = dat_i,
    # finite-sample adjustments:
    ssc = ssc(cluster.adj = TRUE, fixef.K = "nested"),
    cluster = cluster
  )

  # Tidy clustered inference
  vc <- vcov(m)
  tt <- broom::tidy(m, conf.int = TRUE) %>% filter(term == "treat:post")
  
  tibble::tibble(
    proj_id = dat_i$proj_id[1],
    n_obs   = nrow(dat_i),
    n_plots = dplyr::n_distinct(dat_i$gid),
    n_years = dplyr::n_distinct(dat_i$time_treat),
    att_hat = tt$estimate,
    se_cl   = tt$std.error,
    ci_low  = tt$conf.low,
    ci_high = tt$conf.high,
    p_cl    = tt$p.value
  )
}

# Load matched dataset and run panel regression
prepare_and_run_panel <- function(proj_id_curr,
                                  dir_panel_data,
                                  d_sub,
                                  xvars = c("dist_degra"),
                                  time_treat_min = -5,
                                  cluster_choice = c("gid_fac", "time_treat_fac"),
                                  fixefs = c("gid_fac", "time_treat_fac")) {
  panel_data_file <- file.path(dir_panel_data, paste0("panel_", proj_id_curr, ".RDS"))
  panel <- readRDS(panel_data_file)
  panel %<>% filter(gid %in% d_sub$gid)
  
  panel <- panel %>%
    filter(!is.na(z), !is.na(gid), !is.na(time_treat), !is.na(treat), !is.na(post)) %>%
    filter(time_treat >= time_treat_min) %>%
    mutate(time_treat_fac = factor(time_treat),
           gid_fac = factor(gid))
  
  if (all(panel$z == 0)) {
    tibble::tibble(proj_id = proj_id_curr,
                   ate = 0,
                   ate_se = 0)
  } else {
    results <- panel_data_fit_one_project(panel,
                                         y = "z",
                                         xvars = xvars,
                                         fixefs = fixefs,
                                         cluster = cluster_choice)
    results %>%
      mutate(ate = att_hat, ate_se = se_cl) %>%
      select(proj_id, ate, ate_se)
  }
}
  
