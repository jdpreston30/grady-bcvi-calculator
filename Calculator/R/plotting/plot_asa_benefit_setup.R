plot_asa_benefit_setup <- function(coefs_tbl, platt_model, patient_data) {
  Age <- patient_data$Age
  max_carotid <- max(patient_data$Max_RC, patient_data$Max_LC, na.rm = TRUE)
  max_vert <- max(patient_data$Max_RV, patient_data$Max_LV, patient_data$Max_VB, na.rm = TRUE)

  # Determine which injury to sweep
  sweep_target <- if (max_carotid >= max_vert) "carotid" else "vert"
  fixed_carotid <- max_carotid
  fixed_vert <- max_vert

  scenario <- dplyr::case_when(
    sweep_target == "carotid" ~ "Carotid",
    sweep_target == "vert" ~ "Vertebral",
    TRUE ~ "Other"
  )

  coefs <- setNames(coefs_tbl$Coefficient, coefs_tbl$Variable)
  coef_safe <- function(name) ifelse(name %in% names(coefs), coefs[[name]], 0)

  asa_risks <- tibble::tibble(
    ASA = c(0, 1),
    max_carotid = max_carotid,
    age_max_vert = Age * max_vert
  ) %>%
    dplyr::mutate(
      logit = coef_safe("(Intercept)") +
        coef_safe("ASA") * ASA +
        coef_safe("max_carotid") * max_carotid +
        coef_safe("age_max_vert") * age_max_vert,
      prob = plogis(logit),
      scaled_prob = predict(platt_model, newdata = tibble(prob = prob), type = "response") * 100,
      line_type = paste0("ASA ", ASA),
      injury_grade = if (sweep_target == "carotid") max_carotid else max_vert,
      scenario = scenario
    )

  # Generate pred_grid centered on actual patient pattern
  if (sweep_target == "carotid") {
    pred_grid <- tibble::tibble(max_carotid = 1:5, max_vert = fixed_vert)
  } else {
    pred_grid <- tibble::tibble(max_carotid = fixed_carotid, max_vert = 1:5)
  }

  pred_grid <- pred_grid %>%
    tidyr::crossing(ASA = c(0, 1)) %>%
    mutate(
      age_max_vert = Age * max_vert,
      injury_grade = if (sweep_target == "carotid") max_carotid else max_vert
    ) %>%
    mutate(
      logit = coef_safe("(Intercept)") +
        coef_safe("ASA") * ASA +
        coef_safe("max_carotid") * max_carotid +
        coef_safe("age_max_vert") * age_max_vert,
      prob = plogis(logit),
      scaled_prob = predict(platt_model, newdata = tibble(prob = prob), type = "response") * 100,
      line_type = paste0("ASA ", ASA)
    )

  list(asa_risks = asa_risks, pred_grid = pred_grid)
}
