fit_cat_quant_model <- function(d_sub) {
  subclass_unique <- unique(d_sub$subclass)
  ate_subclass_df <- data.frame()
  subclass_n <- c(table(d_sub$subclass))
  for (subclass_curr in subclass_unique) {
    res_df_curr <- data.frame()
    s0_unique <- unique(d_sub$s0)
    for (s0_curr in c(s0_unique, "All")) {
      d_sub_cat_quant <- d_sub %>%
        filter(subclass == subclass_curr)
      if (s0_curr != "All") {
        d_sub_cat_quant %<>%
          filter(pre_id == s0_curr)
      }
      for (f.treat_curr in c("Treatment", "Control")) {
        d_sub_cat_quant_treat_curr <- d_sub_cat_quant %>% 
          filter(f.treat == f.treat_curr)
        d_sub_cat_quant_treat_curr_s1_1 <- d_sub_cat_quant_treat_curr %>%
          filter(s1 == "1")
        diff_vec <- d_sub_cat_quant_treat_curr_s1_1 %>% 
          pull(difference)
        add <- data.frame(
          subclass = subclass_curr,
          n_in_subclass = NROW(d_sub_cat_quant),
          vcs_id = vcs,
          s0 = s0_curr,
          s1 = "1", 
          subclass = subclass_curr,
          subclass_weight = subclass_weights[as.character(subclass_curr)],
          f.treat = f.treat_curr,
          n = NROW(d_sub_cat_quant_treat_curr_s1_1),
          n_tot = NROW(d_sub_cat_quant_treat_curr),
          mu_hat = mean(diff_vec),
          se_mu_hat = sd(diff_vec) / sqrt(length(diff_vec)))
        add$pi_hat = add$n / add$n_tot
        add$se_pi_hat <- sqrt(add$pi_hat * (1 - add$pi_hat) / add$n_tot)
        res_df_curr <- rbind(res_df_curr, add)
      }
    }
    ate_df_add <- res_df_curr %>% 
      select(c("vcs_id", "subclass", "n_in_subclass", "s0", "f.treat", "mu_hat", "se_mu_hat", "pi_hat", "se_pi_hat")) %>%
      group_by(vcs_id, s0) %>%
      pivot_wider(names_from = f.treat, 
                  values_from =  c("mu_hat", "se_mu_hat", "pi_hat", "se_pi_hat")) %>%
      mutate(ate = pi_hat_Treatment * mu_hat_Treatment - pi_hat_Control * mu_hat_Control) %>%
      mutate(ate_se = sqrt((se_mu_hat_Treatment^2 + mu_hat_Treatment^2) * 
                             (se_pi_hat_Treatment^2 + pi_hat_Treatment^2) - 
                             mu_hat_Treatment^2 * pi_hat_Treatment^2 + 
                             (se_mu_hat_Control^2 + mu_hat_Control^2) * 
                             (se_pi_hat_Control^2 + pi_hat_Control^2) - 
                             mu_hat_Control^2 * pi_hat_Control^2)) %>%
      select(c("vcs_id", "subclass", "n_in_subclass", "s0", "ate", "ate_se")) %>%
      ungroup()
    ate_subclass_df <- rbind(ate_subclass_df, ate_df_add)  
  }
  
  ate_subclass_df %<>%
    filter(!is.na(ate) & !is.na(ate_se))
  ate_estimates_out <- ate_subclass_df %>% 
    group_by(vcs_id, s0) %>%
    mutate(subclass_weight = n_in_subclass / sum(n_in_subclass)) %>%
    summarise(ate = sum(subclass_weight * ate, na.rm = T),
              ate_se = sqrt(sum(subclass_weight^2 * ate_se^2)),
              .groups = "keep") %>%
    ungroup()
  return(ate_estimates_out)
}




# 
# fit_cat_quant_model <- function(d_sub) {
#   res_df_curr <- data.frame()
#   for (subclass_curr in subclass_unique) {
#     #########################################################
#     # Fit categorical/quantitative model
#     for (s0_prop_forested_curr in pre_id_unique) {
#       d_sub_cat_quant <- d_sub %>%
#         filter(subclass == subclass_curr) %>%
#         filter(pre_id == s0_prop_forested_curr)
#       for (f.treat_curr in c("Treatment", "Control")) {
#         s1_tab <- d_sub_cat_quant %>% 
#           filter(f.treat == f.treat_curr) %>%
#           pull(s1) %>%
#           table()
#         for (s1_curr in names(s1_tab)) {
#           diff_vec <- d_sub_cat_quant %>% 
#             filter(f.treat == f.treat_curr) %>%
#             filter(s1 == s1_curr) %>%
#             pull(difference)
#           add <- data.frame(
#             vcs_id = vcs,
#             s1 = s1_curr, 
#             pre_id = s0_prop_forested_curr,
#             subclass = subclass_curr,
#             subclass_weight = subclass_weights[as.character(subclass_curr)],
#             f.treat = f.treat_curr,
#             n = s1_tab[s1_curr],
#             n_tot = sum(s1_tab),
#             pi_hat = s1_tab[s1_curr] / sum(s1_tab),
#             mu_hat = mean(diff_vec),
#             se_mu_hat = ifelse(s1_curr == "0", 0, sd(diff_vec) / sqrt(length(diff_vec))))
#           add$se_pi_hat <- sqrt(add$pi_hat * (1 - add$pi_hat) / add$n_tot)
#           res_df_curr <- rbind(res_df_curr, add)
#         }
#       }
#     }
#   }
#   res_df <- res_df_curr %>%
#     group_by(vcs_id, s1, pre_id, f.treat) %>%
#     summarise(pi_hat_agg = sum(pi_hat * subclass_weight, na.rm = T), 
#               se_pi_hat_agg = sqrt(sum(se_pi_hat^2 * subclass_weight^2, na.rm = T)),
#               mu_hat_agg = sum(mu_hat * subclass_weight, na.rm = T), 
#               se_mu_hat_agg = sqrt(sum(se_mu_hat^2 * subclass_weight^2, na.rm = T)),
#               n_agg = sum(n_tot),
#               .groups = "keep") %>%
#     arrange(pre_id, f.treat, s1)
#   return(res_df)
# }
