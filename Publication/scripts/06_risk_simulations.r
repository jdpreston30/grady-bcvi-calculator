#* 6: Risk Simulations
#+ 6.1: Setup and preparation
#- 6.1.1: Create rf_xgb dataset (needed for mean age calculation)
rf_xgb <- ml_modeling_data %>%
  select(-c(tot_vert_inj, max_carotid, tot_carotid_inj, max_vert), -ID)
#- 6.1.2: Set model shortcut
model_obj <- lasso_all_variants$weighted
#- 6.1.3: Fit Platt scaling model from CV predictions
cv_preds <- model_obj$preds_cv %>%
  filter(!is.na(prob), !is.na(truth)) %>%
  mutate(truth_bin = as.numeric(truth == "Y"))
platt_model <- glm(truth_bin ~ prob, data = cv_preds, family = binomial())
#+ 6.2: Generate prediction grid and model coefficients
#- 6.2.1: Generate prediction grid (ASA = 0 and 1)
mean_age <- round(mean(rf_xgb$age), digits = 0)
pred_data_all <- rbind(
  expand.grid(max_carotid = 0:5, max_vert = 0, ASA = 0:1, scenario = "Carotid"),
  expand.grid(max_carotid = 0, max_vert = 0:5, ASA = 0:1, scenario = "Vertebral"),
  expand.grid(max_carotid = 0:5, max_vert = 0:5, ASA = 0:1, scenario = "Combined")
) %>%
  mutate(
    sexM = 0,
    age_ASA = mean_age * ASA,
    age_max_vert = mean_age * max_vert
  )
#- 6.2.2: Extract coefficients from weighted model
coefs <- model_obj$selected_variables
coef_vals <- setNames(coefs$Coefficient, coefs$Variable)
coef_safe <- function(name) ifelse(name %in% names(coef_vals), coef_vals[[name]], 0)
#+ 6.3: Compute predictions and prepare plot data
#- 6.3.1: Compute predicted probabilities
pred_data_all <- pred_data_all %>%
  mutate(
    logit = coef_safe("(Intercept)") +
      coef_safe("ASA") * ASA +
      coef_safe("sexM") * sexM +
      coef_safe("max_carotid") * max_carotid +
      coef_safe("age_ASA") * age_ASA +
      coef_safe("age_max_vert") * age_max_vert,
    predicted_prob = 100 * (1 / (1 + exp(-logit))),
    platt_scaled_prob = predict(platt_model, newdata = tibble(prob = predicted_prob / 100), type = "response") * 100
  )
#- 6.3.2: Filter and prepare data for plotting
plot_data <- pred_data_all %>%
  filter(
    (scenario == "Carotid" & max_vert == 0) |
      (scenario == "Vertebral" & max_carotid == 0) |
      (scenario == "Combined" & max_carotid == max_vert)
  ) %>%
  mutate(
    injury_grade = pmax(max_carotid, max_vert),
    scenario = factor(scenario, levels = c("Carotid", "Vertebral", "Combined")),
    line_type = if_else(ASA == 1, "ASA 1", "ASA 0")
  ) %>%
  filter(injury_grade >= 1)
#+ 6.4: Generate visualization plots to compare Platt scaled versus uncalibrated probabilities
#- 6.4.1: Plot uncalibrated probabilities
ggplot(plot_data, aes(x = injury_grade, y = predicted_prob, color = scenario, linetype = line_type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Uncalibrated Stroke Risk by Injury Grade",
    x = "Injury Grade (0 to 5)",
    y = "Predicted Stroke Probability (%)",
    color = "Scenario",
    linetype = "ASA Status"
  ) +
  scale_x_continuous(breaks = 0:5) +
  theme_minimal(base_size = 14)
#- 6.4.2: Plot Platt-scaled probabilities
ggplot(plot_data, aes(x = injury_grade, y = platt_scaled_prob, color = scenario, linetype = line_type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Platt-Scaled Stroke Risk by Injury Grade",
    x = "Injury Grade (0 to 5)",
    y = "Platt-Calibrated Stroke Probability (%)",
    color = "Scenario",
    linetype = "ASA Status"
  ) +
  scale_x_continuous(breaks = 0:5) +
  theme_minimal(base_size = 14)
#+ 6.5: Export data, plot in Prism
write.csv(plot_data, "1C.csv", row.names = FALSE)
#+ 6.6: Prepare models to be exported for risk calculator
saveRDS(model_obj$selected_variables, "../Calculator/data/lasso_weighted_coefs.rds") # raw coefficients
saveRDS(platt_model, "../Calculator/data/platt_model.rds") # platt model
