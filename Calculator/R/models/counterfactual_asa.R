counterfactual_asa_risk_calc_lasso <- function(coefs_tbl, platt_model, max_carotid, max_vert, Sex, Age) {
  coefs <- setNames(coefs_tbl$Coefficient, coefs_tbl$Variable)

  input_df <- tibble(
    ASA = c(0, 1),
    max_carotid = max_carotid,
    age_max_vert = Age * max_vert
  )

  linear_pred <- coefs["(Intercept)"] +
    coefs["ASA"] * input_df$ASA +
    coefs["max_carotid"] * input_df$max_carotid +
    coefs["age_max_vert"] * input_df$age_max_vert

  prob <- predict(platt_model, newdata = tibble(prob = plogis(linear_pred)), type = "response")

  tibble(
    Baseline_Risk = prob[1],
    Risk_If_Given_ASA = prob[2],
    ASA_Benefit = prob[1] - prob[2]
  )
}
