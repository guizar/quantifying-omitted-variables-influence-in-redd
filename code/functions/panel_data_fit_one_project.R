
panel_data_fit_one_project <- function(dat_i,
                                       y = "z",
                                       xvars,
                                       fixefs,
                                       cluster = c("gid"),
                                       offset = NULL) {
  
  rhs <- c("treat:post", xvars)
  
  # Add offset term to RHS if provided
  if (!is.null(offset)) {
    rhs <- c(rhs, paste0("offset(", offset, ")"))
  }
  
  # Build full formula
  fml <- as.formula(
    paste0(
      y, " ~ ",
      paste(rhs, collapse = " + "),
      " | ",
      paste(fixefs, collapse = " + ")
    )
  )
  
  # Fit model
  m <- fixest::feols(
    fml,
    data = dat_i,
    ssc = ssc(cluster.adj = TRUE, fixef.K = "nested"),
    cluster = cluster
  )
  
  return(m)
}



# Tidy panel model results

tidy_panel_results <- function(dat_i,m) {
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
