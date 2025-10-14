
panel_data_fit_one_project <- function(dat_i,
                            y = "z",
                            xvars = xvars,
                            fixefs,
                            cluster = c("gid")) {
  
  rhs <- c("treat:post", xvars)
  # Fixed effects: plot (gid) and relative time (time_treat)
  fml <- as.formula(paste0(y, " ~ ", paste(rhs, collapse = " + "), " | ", paste(fixefs, collapse = " + ")))
  m <- feols(
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


