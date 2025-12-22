
panel_data_fit_one_project <- function(dat_i,
                                       y = "z",
                                      rhs = "treat:post",
                                       xvars=NULL,
                                       fixefs,
                                       cluster = c("gid"),
                                       offset = NULL) {
  
  # Start with base RHS provided as argument (default treat:post)
  rhs <- rhs
  
  # Add additional variables if provided
  if (!is.null(xvars) && length(xvars) > 0) {
    rhs <- c(rhs, xvars)
  }
  
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
  
  # Fit model with error handling
  m <- NULL
  tryCatch({
    m <- suppressWarnings(
      try({
        fixest::feols(
          fml,
          data = dat_i,
          ssc = fixest::ssc(cluster.adj = TRUE, fixef.K = "nested"),
          cluster = cluster
        )
      }, silent = TRUE)
    )
    
    # Check if try() caught an error
    if (inherits(m, "try-error")) {
      error_msg <- as.character(m)
      warning(paste("Model fitting failed for project", dat_i$proj_id[1], ":", error_msg))
      m <- NULL
    }
    
  }, error = function(e) {
    warning(paste("Model fitting failed for project", dat_i$proj_id[1], ":", e$message))
    m <- NULL
  })
  
  return(m)
}


# Tidy panel model results

tidy_panel_results <- function(dat_i, m) {
  # Handle case where model fitting failed
  if (is.null(m)) {
    return(tibble::tibble(
      proj_id = dat_i$proj_id[1],
      n_obs   = nrow(dat_i),
      n_plots = dplyr::n_distinct(dat_i$gid),
      n_years = dplyr::n_distinct(dat_i$time_treat),
      att_hat = 0,
      se_cl   = 0,
      ci_low  = NA_real_,
      ci_high = NA_real_,
      p_cl    = NA_real_
    ))
  }
  
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
