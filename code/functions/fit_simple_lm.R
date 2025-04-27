fit_simple_lm <- function(d, formula = difference ~ f.treat + dist_degra + mean_slp + mean_access + mean_ele + ps) {
  lm_curr <- lm(formula = formula, 
                data = d)
  lm_curr_no_intercept <- update(lm_curr, . ~ . - 1)
  # est_curr <- summary(lm_curr)$coefficients["f.treatTreatment", "Estimate"]
  # est_curr_treat <- summary(lm_curr_no_intercept)$coefficients["f.treatTreatment", "Estimate"]
  # est_curr_control <- summary(lm_curr_no_intercept)$coefficients["f.treatControl", "Estimate"]
  # se_curr <- summary(lm_curr)$coefficients["f.treatTreatment", "Std. Error"]
  # se_curr_treat <- summary(lm_curr_no_intercept)$coefficients["f.treatTreatment", "Std. Error"]
  # se_curr_control <- summary(lm_curr_no_intercept)$coefficients["f.treatControl", "Std. Error"]
  ci_curr <- confint(lm_curr, parm = "f.treatTreatment")
  ci_curr_treat <- confint(lm_curr_no_intercept, parm = "f.treatTreatment")
  ci_curr_control <- confint(lm_curr_no_intercept, parm = "f.treatControl")
  out <- list(diff = data.frame(vcs_id = vcs,
                                    est = summary(lm_curr)$coefficients["f.treatTreatment", "Estimate"],
                                    se = summary(lm_curr)$coefficients["f.treatTreatment", "Std. Error"],
                                    lower = ci_curr[1],
                                    upper = ci_curr[2]),
              treatment = data.frame(vcs_id = vcs,
                                     est = summary(lm_curr_no_intercept)$coefficients["f.treatTreatment", "Estimate"],
                                     se = summary(lm_curr_no_intercept)$coefficients["f.treatTreatment", "Std. Error"],
                                     lower = ci_curr_treat[1],
                                     upper = ci_curr_treat[2]),
              control = data.frame(vcs_id = vcs,
                                   est = summary(lm_curr_no_intercept)$coefficients["f.treatControl", "Estimate"],
                                   se = summary(lm_curr_no_intercept)$coefficients["f.treatControl", "Std. Error"],
                                   lower = ci_curr_control[1],
                                   upper = ci_curr_control[2]))
  return(out)
}
