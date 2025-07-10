#- Examine variable importance in Bayesian
  #! Note, this is using a DIFFERENT package than above
  bayes_data <- ml_modeling_data %>%
    select(-ID)
  bas_model_segments <- BAS::bas.glm(
    stroke ~ ASA + sexM + age + Max_LC + Max_RC + Max_LV + Max_RV + Max_VB + ASA:age + age:Max_LC + age:Max_RC + age:Max_LV + age:Max_RV + age:Max_VB,
    data = bayes_data,
    family = binomial(),
    method = "BAS"
  )
  bas_model_summary <- BAS::bas.glm(
    stroke ~ ASA + sexM + age + max_vert + max_carotid + ASA:age + max_vert:age + max_carotid:age
    data = bayes_data,
    family = binomial(),
    method = "BAS"
  )