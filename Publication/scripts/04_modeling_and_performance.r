#* 4: Run multiple modeling methods
#+ 4.1: Run master pipeline
#! Conditional execution handled inside function per the yaml config settings
all_model_results <- run_all_modeling(
  ml_modeling_data = ml_modeling_data,
  my_seeds_rf = my_seeds_rf,
  seed = 2025,
  config = config
)
#+ 4.2: Extract each model's results for use below (optional unpacking)
lasso_all_variants <- all_model_results$lasso
rf_all_variants <- all_model_results$random_forest
xgb_all_variants <- all_model_results$xgboost
svm_all_variants <- all_model_results$svm
gam_all_variants <- all_model_results$gam
mlp_all_variants <- all_model_results$mlp
bayes_all_variants <- all_model_results$bayes
#+ 4.3: Create summary table of model performance
#- 4.3.1: List all non-Bayes models
model_list <- list(
  LASSO = lasso_all_variants,
  RF    = rf_all_variants,
  XGB   = xgb_all_variants,
  SVM   = svm_all_variants,
  GAM   = gam_all_variants,
  MLP   = mlp_all_variants,
  Bayes = bayes_all_variants
)
#- 4.3.2: Extract summary_cv with 95% CIs from all inner slots
model_summary <- purrr::map_dfr(names(model_list), function(name) {
  extract_cv_summaries_with_ci(name, model_list[[name]])
})
#- 4.3.3: Manually specify the metadata for each model
manual_metadata <- tribble(
  ~Model, ~Method, ~Dataset, ~Weighting, ~Downsampling, ~Category, ~Feature_Selection,
  "SVM_full_downsampled", "SVM", "Full", "N", "Y", "Margin-based classifier", "None",
  "SVM_full_both", "SVM", "Full", "Y", "Y", "Margin-based classifier", "None",
  "SVM_simpl_downsampled", "SVM", "Simple", "N", "Y", "Margin-based classifier", "None",
  "SVM_simpl_both", "SVM", "Simple", "Y", "Y", "Margin-based classifier", "None",
  "Bayes_simpl_down", "Bayes", "Simple", "N", "Y", "Bayes GLM", "None",
  "Bayes_full_down", "Bayes", "Full", "N", "Y", "Bayes GLM", "None",
  "LASSO_weighted", "LASSO", "Simple", "Y", "N", "Penalized GLM", "Built-in",
  "MLP_simpl_down", "MLP", "Simple", "N", "Y", "Neural network", "None",
  "GAM_simpl_down", "GAM", "Simple", "N", "Y", "Additive spline model", "None",
  "RF_rf_down_full", "RF", "Full", "N", "Y", "Ensemble (bagging)", "Implicit",
  "MLP_full_down", "MLP", "Full", "N", "Y", "Neural network", "None",
  "RF_rf_down_simple", "RF", "Simple", "N", "Y", "Ensemble (bagging)", "Implicit",
  "XGB_simpl_down_only", "XGB", "Simple", "N", "Y", "Ensemble (boosting)", "Implicit",
  "XGB_full_down_only", "XGB", "Full", "N", "Y", "Ensemble (boosting)", "Implicit",
  "XGB_full_weight_only", "XGB", "Full", "Y", "N", "Ensemble (boosting)", "Implicit",
  "XGB_simpl_weight_only", "XGB", "Simple", "Y", "N", "Ensemble (boosting)", "Implicit",
  "XGB_full_weight_down", "XGB", "Full", "Y", "Y", "Ensemble (boosting)", "Implicit",
  "XGB_simpl_weight_down", "XGB", "Simple", "Y", "Y", "Ensemble (boosting)", "Implicit",
  "XGB_simpl_unweight_nodown", "XGB", "Simple", "N", "N", "Ensemble (boosting)", "Implicit",
  "SVM_full_weighted", "SVM", "Full", "Y", "N", "Margin-based classifier", "None",
  "SVM_full_unadjusted", "SVM", "Full", "N", "N", "Margin-based classifier", "None",
  "GAM_simpl_nodown", "GAM", "Simple", "N", "N", "Additive spline model", "None",
  "RF_rf_nodown_simple", "RF", "Simple", "N", "N", "Ensemble (bagging)", "Implicit",
  "SVM_simpl_weighted", "SVM", "Simple", "Y", "N", "Margin-based classifier", "None",
  "SVM_simpl_unadjusted", "SVM", "Simple", "N", "N", "Margin-based classifier", "None",
  "Bayes_simpl_nodown", "Bayes", "Simple", "N", "N", "Bayes GLM", "None",
  "Bayes_full_nodown", "Bayes", "Full", "N", "N", "Bayes GLM", "None",
  "XGB_full_unweight_nodown", "XGB", "Full", "N", "N", "Ensemble (boosting)", "Implicit",
  "MLP_full_nodown", "MLP", "Full", "N", "N", "Neural network", "None",
  "RF_rf_nodown_full", "RF", "Full", "N", "N", "Ensemble (bagging)", "Implicit",
  "MLP_simpl_nodown", "MLP", "Simple", "N", "N", "Neural network", "None",
  "LASSO_unweighted", "LASSO", "Simple", "N", "N", "Penalized GLM", "Built-in"
)
#- 4.3.4: Combine and annotate with metadata for each model
all_model_summary <- model_summary %>%
  mutate(
    Youdens_J = Sensitivity + Specificity - 1
  ) %>%
  left_join(manual_metadata, by = "Model") %>%
  mutate(
    # Format metrics with 95% CIs in brackets
    AUC_formatted = sprintf("%.2f [%.2f–%.2f]", AUC, AUC_CI_lower, AUC_CI_upper),
    Sensitivity_formatted = sprintf("%d%% [%d%%–%d%%]", 
                                    round(Sensitivity * 100, 0), 
                                    round(Sens_CI_lower * 100, 0), 
                                    round(Sens_CI_upper * 100, 0)),
    Specificity_formatted = sprintf("%d%% [%d%%–%d%%]", 
                                    round(Specificity * 100, 0), 
                                    round(Spec_CI_lower * 100, 0), 
                                    round(Spec_CI_upper * 100, 0))
  ) %>%
  mutate(
    Sampling_Method = paste0(
      if_else(Dataset == "Full", "F", "S"),
      case_when(
        Weighting == "Y" & Downsampling == "Y" ~ ",W,D",
        Weighting == "Y" & Downsampling == "N" ~ ",W",
        Weighting == "N" & Downsampling == "Y" ~ ",D",
        TRUE ~ ""
      )
    )) %>%
  group_by(Method) %>%
  arrange(desc(Youdens_J), .by_group = TRUE) %>%
  ungroup() %>%
  mutate(
    across(where(is.double) & !c(Sensitivity, Specificity, AUC, 
                                  Sens_CI_lower, Sens_CI_upper, 
                                  Spec_CI_lower, Spec_CI_upper, 
                                  AUC_CI_lower, AUC_CI_upper, Youdens_J), ~ round(.x, 2))
  ) %>%
  select(Method, Category, Feature_Selection, Youdens_J, 
          AUC, AUC_CI_lower, AUC_CI_upper, AUC_formatted,
          Sensitivity, Sens_CI_lower, Sens_CI_upper, Sensitivity_formatted,
          Specificity, Spec_CI_lower, Spec_CI_upper, Specificity_formatted,
          Model, Dataset, Weighting, Downsampling, Sampling_Method) %>%
  arrange(desc(Youdens_J), desc(AUC), desc(Sensitivity), desc(Specificity)) %>%
  mutate( # Added after manual inspection and choice of model
    Chosen = if_else(
      Model %in% c(
        "LASSO_weighted",
        "SVM_simpl_downsampled",
        "Bayes_full_down",
        "GAM_simpl_down",
        "RF_rf_down_full",
        "XGB_simpl_down_only",
        "MLP_full_down"
      ),
      "Y", NA_character_
    )
  ) %>%
  mutate(
    Youdens_J = round(Youdens_J, 2),  # Round after all sorting is complete
    Dataset = case_when(
      Dataset == "Simple" ~ "Simplified",
      Dataset == "Full" ~ "Complete",
      TRUE ~ Dataset
    )
  ) %>%
  mutate(
    Method = case_when(
      Method == "Bayes" ~ "BLR",
      Method == "XGB" ~ "GBM",
      TRUE ~ Method
    )
  ) 
#- 4.3.5: Construct and export ST3 (with 95% CIs in brackets)
#! Note that when running on desktop, results are consistent with the original submission; however, when running on laptop, they differ slightly. Therefore, for the publication results, we will use desktop-generated results only.
ST3 <- all_model_summary %>%
  mutate("Dataset†; Weighting; Downsampling" = paste(Dataset, Weighting, Downsampling, sep = "; ")) %>%
  select(Method, Youdens_J, AUC_formatted, Sensitivity_formatted, Specificity_formatted, "Dataset†; Weighting; Downsampling") %>%
  rename("Machine Learning Model" = Method,
          "Youden's J Statistic*" = Youdens_J,
          "AUC (95% CI)" = AUC_formatted,
          "Sensitivity (95% CI)" = Sensitivity_formatted,
          "Specificity (95% CI)" = Specificity_formatted) |>
  filter(`Machine Learning Model` != "RF")
#- 4.3.6: Export ST3
write.xlsx(ST3, "Outputs/Tables/ST3.xlsx")
#- 4.3.7: Structure T2
T2 <- all_model_summary %>%
  filter(Chosen == "Y") %>%
  select(Method, Category, Feature_Selection, Youdens_J, AUC_formatted, Sensitivity_formatted, Specificity_formatted) %>%
  rename(
    "Machine Learning Model*" = Method,
    "Feature Selection" = Feature_Selection,
    "Youden's J Statistic†" = Youdens_J,
    "AUC (95% CI)" = AUC_formatted,
    "Sensitivity (95% CI)" = Sensitivity_formatted,
    "Specificity (95% CI)" = Specificity_formatted
  ) |>
  filter(`Machine Learning Model*` != "RF")
  #! Filtered out RF due to seed reproducibility issues as detailed in methods section
#- 4.3.8: Export T2
write.xlsx(T2, "Outputs/Tables/T2.xlsx")
#+ 4.4: Document variables and interactions used by each modeling method
document_model_variables(ml_modeling_data)