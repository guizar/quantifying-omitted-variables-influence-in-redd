fit_spatial_lme_subclass <- function(d_sub,
                                   formula = difference ~ f.treat) {
  res_df_lm_curr <- data.frame()
  for (subclass_curr in subclass_unique) {
    #########################################################
    # Fit basic linear model ignoring categorical aspect
    d_sub_lm <- d_sub %>%
      filter(subclass == subclass_curr)
    try_error <- 
      try (
        expr = {
          lm_curr <- nlme::gls(model = formula, 
                             data = d_sub_lm,
                             correlation = nlme::corGaus(form = ~ x + y))
          coef_mat <- summary(lm_curr)$tTable
          add_lm <- data.frame(
            proj_id = d_sub$proj_id[1],
            subclass = subclass_curr,
            subclass_weight = subclass_weights[as.character(subclass_curr)],
            n_tot = nrow(d_sub_lm),
            ate = coef_mat["treat", "Value"],
            ate_se = coef_mat["treat", "Std.Error"])
        }, 
        silent = TRUE
      )
    if (inherits(x = try_error, what = "try-error")) {
      lm_curr <- lm(formula = formula, data = d_sub_lm)
      coef_mat <- summary(lm_curr)$coefficients
      add_lm <- data.frame(
        proj_id = d_sub$proj_id[1],
        subclass = subclass_curr,
        subclass_weight = subclass_weights[as.character(subclass_curr)],
        n_tot = nrow(d_sub_lm),
        ate = coef_mat["treat", "Estimate"],
        ate_se = coef_mat["treat", "Std. Error"])
        # mu_hat = coef_mat["f.treatTreatment", "Estimate"],
        # se_mu_hat = coef_mat["f.treatTreatment", "Std. Error"])
    }
    res_df_lm_curr <- rbind(res_df_lm_curr, add_lm)
  }
  res_df_lm <- res_df_lm_curr %>%
    group_by(proj_id) %>%
    summarise(ate_agg = sum(ate * subclass_weight, na.rm = T), 
              ate_se_agg = sqrt(sum(ate_se^2 * subclass_weight^2, na.rm = T)),
              n_agg = sum(n_tot),
              .groups = "keep") %>%
    ungroup() %>%
    mutate(ate = ate_agg) %>%
    mutate(ate_se = ate_se_agg)
  return(res_df_lm)
}



# 
# fit_spatial_lme_subclass <- function(d_sub,
#                                      formula = difference ~ f.treat) {
#   res_df_lm_curr <- data.frame()
#   for (subclass_curr in subclass_unique) {
#     #########################################################
#     # Fit basic linear model ignoring categorical aspect
#     d_sub_lm <- d_sub %>%
#       filter(subclass == subclass_curr)
#     try_error <- 
#       try (
#         expr = {
#           lm_curr <- nlme::gls(model = formula, 
#                                data = d_sub_lm,
#                                correlation = nlme::corGaus(form = ~ x + y))
#           coef_mat <- summary(lm_curr)$tTable
#           add_lm <- data.frame(
#             vcs_id = vcs,
#             subclass = subclass_curr,
#             subclass_weight = subclass_weights[as.character(subclass_curr)],
#             n_tot = nrow(d_sub_lm),
#             mu_hat = coef_mat["f.treatTreatment", "Value"],
#             se_mu_hat = coef_mat["f.treatTreatment", "Std.Error"])
#         }, 
#         silent = TRUE
#       )
#     if (inherits(x = try_error, what = "try-error")) {
#       lm_curr <- lm(formula = formula, data = d_sub_lm)
#       coef_mat <- summary(lm_curr)$coefficients
#       add_lm <- data.frame(
#         vcs_id = vcs,
#         subclass = subclass_curr,
#         subclass_weight = subclass_weights[as.character(subclass_curr)],
#         n_tot = nrow(d_sub_lm),
#         mu_hat = coef_mat["f.treatTreatment", "Estimate"],
#         se_mu_hat = coef_mat["f.treatTreatment", "Std. Error"])
#     }
#     res_df_lm_curr <- rbind(res_df_lm_curr, add_lm)
#   }
#   res_df_lm <- res_df_lm_curr %>%
#     group_by(vcs_id) %>%
#     summarise(mu_hat_agg = sum(mu_hat * subclass_weight, na.rm = T), 
#               se_mu_hat_agg = sqrt(sum(se_mu_hat^2 * subclass_weight^2, na.rm = T)),
#               n_agg = sum(n_tot),
#               .groups = "keep")
#   return(res_df_lm)
# }
