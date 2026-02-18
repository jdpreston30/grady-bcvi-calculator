#* 5: ROC construction
#+ 5.1: Bind all data together from the chosen models
roc_data <- bind_rows(
  mlp_all_variants$full_down$preds_cv %>% mutate(model = "MLP_full_down"),
  gam_all_variants$simpl_down$preds_cv %>% mutate(model = "GAM_simpl_down"),
  rf_all_variants$rf_down_full$preds_cv %>% mutate(model = "RF_rf_down_full"),
  bayes_all_variants$full_down$preds_cv %>% mutate(model = "Bayes_full_down"),
  svm_all_variants$simpl_downsampled$preds_cv %>% mutate(model = "SVM_simpl_downsampled"),
  lasso_all_variants$weighted$preds_cv %>% mutate(model = "LASSO_weighted"),
  xgb_all_variants$simpl_down_only$preds_cv %>% mutate(model = "XGB_simpl_down_only")
)
#+ 5.2: Create ROC list with relevant info for each model
roc_list <- roc_data %>%
  group_by(model) %>%
  group_map(~ {
    roc_obj <- pROC::roc(
      response = .x$truth,
      predictor = .x$prob,
      levels = c("N", "Y"),
      direction = "<"
    )
    tibble(
      model = .y$model,
      specificity = rev(roc_obj$specificities),
      sensitivity = rev(roc_obj$sensitivities)
    )
  }) %>%
  bind_rows()
#+ 5.3: Set required parameters
#- 5.3.1: Define model labels
model_label_map <- c(
  "LASSO_weighted"        = "LASSO",
  "SVM_simpl_downsampled" = "SVM",
  "Bayes_full_down"       = "BLR",
  "GAM_simpl_down"        = "GAM",
  "RF_rf_down_full"       = "RF",
  "XGB_simpl_down_only" = "GBM",
  "MLP_full_down" = "MLP"
)
#- 5.3.2: Order model labels
ordered_models <- names(model_label_map)
#- 5.3.3: Define common FPR sequence
common_x <- seq(0, 1, length.out = 100)
#- 5.3.4: Lookup AUCs from summary table
auc_lookup <- all_model_summary %>%
  filter(Model %in% ordered_models) %>%
  select(Model, AUC) %>%
  mutate(AUC = format(round(AUC, 2), nsmall = 2)) %>%
  deframe()
#+ 5.4: Interpolate & relabel to construct ROC curves for Prism
roc_list_interpolated <- roc_list %>%
  mutate(
    model_short = model_label_map[as.character(model)],
    fpr = 1 - specificity
  ) %>%
  group_by(model, model_short) %>%
  summarise(
    fpr_interp = list(common_x),
    sens_interp = list(approx(fpr, sensitivity, xout = common_x, rule = 2)$y),
    .groups = "drop"
  ) %>%
  mutate(
    auc = auc_lookup[as.character(model)],
    label = paste0(model_short, " (AUC=", auc, ")")
  ) %>%
  mutate(
    model = factor(model, levels = ordered_models)
  ) %>%
  arrange(model) %>%
  unnest(c(fpr_interp, sens_interp)) %>%
  rename(FPR = fpr_interp, sensitivity = sens_interp) %>%
  select(FPR, label, sensitivity) %>%
  pivot_wider(
    names_from = label,
    values_from = sensitivity
  ) %>%
  arrange(FPR)
#+ 5.5: Export to CSV for Prism
write.csv(roc_list_interpolated, "Outputs/Figures/1B.csv", row.names = FALSE)
