# d_sub = d_sub
# formula = outcome_prop ~ treat
# method = "ps_subclass"


fit_simple_lm_subclass <- function(d_sub,
                                   formula = difference ~ f.treat,
                                   method = "ps_subclass",
                                   ps_weights = NULL) {
  if (method == "ps_subclass") {
    subclass_n <- c(table(d_sub$subclass))
    d_sub$n_in_subclass <- subclass_n[as.character(d_sub$subclass)]
    d_sub$subclass_weight <- d_sub$n_in_subclass / sum(subclass_n)
    res_df_lm_curr <- data.frame()
    for (subclass_curr in subclass_unique) {
      #########################################################
      # Fit basic linear model ignoring categorical aspect
      d_sub_lm <- d_sub %>%
        filter(subclass == subclass_curr)
      lm_curr <- lm(formula = formula, data = d_sub_lm)
      coef_mat <- summary(lm_curr)$coefficients
      add_lm <- data.frame(
        proj_id = d_sub$proj_id[1],
        subclass = subclass_curr,
        subclass_weight = subclass_weights[as.character(subclass_curr)],
        n_tot = nrow(d_sub_lm),
        ate = coef_mat["treat", "Estimate"],
        ate_se = coef_mat["treat", "Std. Error"])
      res_df_lm_curr <- rbind(res_df_lm_curr, add_lm)
    }
    res_df_lm <- res_df_lm_curr %>%
      group_by(proj_id) %>%
      summarise(ate = sum(ate * subclass_weight, na.rm = T), 
                ate_se = sqrt(sum(ate_se^2 * subclass_weight^2, na.rm = T)),
                n_agg = sum(n_tot),
                .groups = "keep")
  }
  if (method %in% c("ps_weights", "simple")) {
    # d_sub$ps_weights <- NA
    # d_sub %<>%
    # mutate(ps_weights = ifelse(f.treat == "Treatment", 1 / ps, 1 / (1 - ps)))
    if (method == "ps_weights") {
      # weights <- d_sub$ps_weights
      lm_curr <- lm(formula = as.formula(formula), 
                    data = d_sub,
                    weights = ps_weights)
    }
    if (method == "simple") {
      lm_curr <- lm(formula = formula, 
                    data = d_sub)
    }
    coef_mat <- summary(lm_curr)$coefficients
    res_df_lm <- data.frame(
      proj_id = d_sub$proj_id[1],
      n_agg = nrow(d_sub),
      ate = coef_mat["treat", "Estimate"],
      ate_se = coef_mat["treat", "Std. Error"])
  }
  return(res_df_lm)
}









# 
# fit_simple_lm_subclass <- function(d_sub,
#                                    formula = difference ~ f.treat,
#                                    method = "ps_subclass") {
#   subclass_n <- c(table(d_sub$subclass))
#   if (method == "ps_subclass") {
#     d_sub$n_in_subclass <- subclass_n[as.character(d_sub$subclass)]
#     d_sub$subclass_weight <- d_sub$n_in_subclass / sum(subclass_n)
#     res_df_lm_curr <- data.frame()
#     for (subclass_curr in subclass_unique) {
#       #########################################################
#       # Fit basic linear model ignoring categorical aspect
#       d_sub_lm <- d_sub %>%
#         filter(subclass == subclass_curr)
#       lm_curr <- lm(formula = formula, data = d_sub_lm)
#       coef_mat <- summary(lm_curr)$coefficients
#       add_lm <- data.frame(
#         vcs_id = vcs,
#         subclass = subclass_curr,
#         subclass_weight = subclass_weights[as.character(subclass_curr)],
#         n_tot = nrow(d_sub_lm),
#         ate = coef_mat["f.treatTreatment", "Estimate"],
#         ate_se = coef_mat["f.treatTreatment", "Std. Error"])
#       res_df_lm_curr <- rbind(res_df_lm_curr, add_lm)
#     }
#     res_df_lm <- res_df_lm_curr %>%
#       group_by(vcs_id) %>%
#       summarise(ate = sum(ate * subclass_weight, na.rm = T), 
#                 ate_se = sqrt(sum(ate_se^2 * subclass_weight^2, na.rm = T)),
#                 n_agg = sum(n_tot),
#                 .groups = "keep")
#   }
#   if (method == "ps_weights") {
#     # d_sub$ps_weights <- NA
#     # d_sub %<>%
#     # mutate(ps_weights = ifelse(f.treat == "Treatment", 1 / ps, 1 / (1 - ps)))
#     lm_curr <- lm(formula = formula, 
#                   data = d_sub,
#                   weights = d_sub$ps_weights)
#     coef_mat <- summary(lm_curr)$coefficients
#     res_df_lm <- data.frame(
#       vcs_id = vcs,
#       n_agg = nrow(d_sub),
#       ate = coef_mat["f.treatTreatment", "Estimate"],
#       ate_se = coef_mat["f.treatTreatment", "Std. Error"])
#   }
#   return(res_df_lm)
# }
