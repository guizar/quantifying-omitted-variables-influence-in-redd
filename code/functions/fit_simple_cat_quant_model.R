fit_simple_cat_quant_model <- function(d_sub, mu_pri_mean = .1, mu_pri_sd = .05) {
  subclass_unique <- unique(d_sub$subclass)
  ate_subclass_df <- tibble()
  subclass_n <- c(table(d_sub$subclass))
  for (subclass_curr in subclass_unique) {#subclass_curr <- subclass_unique[1]#
    res_df_curr <- tibble()
    d_sub_cat_quant <- d_sub %>%
    filter(subclass == subclass_curr)
    for (f.treat_curr in c(1, 0)) {
      d_sub_cat_quant_treat_curr <- d_sub_cat_quant %>% 
        filter(treat == f.treat_curr)
      d_sub_cat_quant_treat_curr$s1 <- ifelse(d_sub_cat_quant_treat_curr$outcome_prop == 0, "0", "1")
      d_sub_cat_quant_treat_curr_s1_1 <- d_sub_cat_quant_treat_curr %>%
        filter(s1 == "1")
      diff_vec <- d_sub_cat_quant_treat_curr_s1_1 %>% 
        pull(outcome_prop)
      add <- data.frame(
        subclass = subclass_curr,
        n_in_subclass = NROW(d_sub_cat_quant),
        proj_id = d_sub$proj_id[1],
        subclass = subclass_curr,
        subclass_weight = subclass_weights[as.character(subclass_curr)],
        treat = f.treat_curr,
        n = NROW(d_sub_cat_quant_treat_curr_s1_1),
        n_tot = NROW(d_sub_cat_quant_treat_curr),
        mu_hat = mean(diff_vec),
        se_mu_hat = sd(diff_vec) / sqrt(length(diff_vec)))
      if(is.na(add$mu_hat)) {
        add$mu_hat <- mu_pri_mean
      }
      if(is.na(add$se_mu_hat)) {
        add$se_mu_hat <- mu_pri_sd
      }
      add$pi_hat = add$n / add$n_tot
      add$se_pi_hat <- sqrt(add$pi_hat * (1 - add$pi_hat) / add$n_tot)
      res_df_curr <- rbind(res_df_curr, add)
    }
    
    ate_df_add <- res_df_curr %>% 
      select(c("proj_id", "subclass", "n_in_subclass", "treat", "mu_hat", "se_mu_hat", "pi_hat", "se_pi_hat")) %>%
      group_by(proj_id) %>%
      pivot_wider(names_from = treat, 
                  values_from =  c("mu_hat", "se_mu_hat", "pi_hat", "se_pi_hat")) %>%
      mutate(ate = pi_hat_1 * mu_hat_1 - pi_hat_0 * mu_hat_0) %>%
      mutate(ate_se = sqrt((se_mu_hat_1^2 + mu_hat_1^2) * 
                             (se_pi_hat_1^2 + pi_hat_1^2) - 
                             mu_hat_1^2 * pi_hat_1^2 + 
                             (se_mu_hat_0^2 + mu_hat_0^2) * 
                             (se_pi_hat_0^2 + pi_hat_0^2) - 
                             mu_hat_0^2 * pi_hat_0^2)) %>%
      select(c("proj_id", "subclass", "n_in_subclass", "ate", "ate_se")) %>%
      ungroup()
    ate_subclass_df <- rbind(ate_subclass_df, ate_df_add)  
  }
  
  ate_subclass_df %<>%
    filter(!is.na(ate) & !is.na(ate_se))
  ate_estimates_out <- ate_subclass_df %>% 
    group_by(proj_id) %>%
    mutate(subclass_weight = n_in_subclass / sum(n_in_subclass)) %>%
    summarise(ate = sum(subclass_weight * ate, na.rm = T),
              ate_se = sqrt(sum(subclass_weight^2 * ate_se^2)),
              .groups = "keep") %>%
    ungroup()
  return(ate_estimates_out)
}
