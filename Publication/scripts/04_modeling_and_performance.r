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
#- 4.4.1: Define variable structures for each dataset
ml_modeling_data_vars <- names(ml_modeling_data)
rf_xgb_vars <- ml_modeling_data %>% select(-c(tot_vert_inj, max_carotid, tot_carotid_inj, max_vert), -ID) %>% names()
rf_xgb_simpl_vars <- ml_modeling_data %>% select(stroke, ASA, sexM, age, max_carotid, max_vert) %>% names()
#- 4.4.2: Define interaction terms for each method
lasso_interactions <- c("age_ASA", "age_max_vert", "age_max_carotid")
gam_interactions <- c("age_ASA", "age_max_vert", "age_max_carotid")
bayes_full_interactions <- c("ASA:age", "age:Max_LC", "age:Max_RC", "age:Max_LV", "age:Max_RV")
bayes_simpl_interactions <- c("ASA:age", "max_vert:age", "max_carotid:age")
#- 4.4.3: Create detailed tibble of variables by method
variable_documentation <- tribble(
  ~Method, ~Dataset, ~Base_Variables, ~N_Base, ~Interaction_Terms, ~N_Interactions, ~Total_Features, ~Notes,
  "LASSO", "Simple", paste(setdiff(rf_xgb_simpl_vars, "stroke"), collapse = ", "), 
    length(setdiff(rf_xgb_simpl_vars, "stroke")), paste(lasso_interactions, collapse = ", "), 
    length(lasso_interactions), length(setdiff(rf_xgb_simpl_vars, "stroke")) + length(lasso_interactions), 
    "Manually specified age-based interactions",
  "GAM", "Simple", paste(setdiff(rf_xgb_simpl_vars, "stroke"), collapse = ", "), 
    length(setdiff(rf_xgb_simpl_vars, "stroke")), paste(gam_interactions, collapse = ", "), 
    length(gam_interactions), length(setdiff(rf_xgb_simpl_vars, "stroke")) + length(gam_interactions), 
    "Manually specified age-based interactions",
  "Random Forest", "Full", paste(setdiff(rf_xgb_vars, "stroke"), collapse = ", "), 
    length(setdiff(rf_xgb_vars, "stroke")), "None (implicit)", 0, 
    length(setdiff(rf_xgb_vars, "stroke")), "Tree splits capture interactions automatically",
  "Random Forest", "Simple", paste(setdiff(rf_xgb_simpl_vars, "stroke"), collapse = ", "), 
    length(setdiff(rf_xgb_simpl_vars, "stroke")), "None (implicit)", 0, 
    length(setdiff(rf_xgb_simpl_vars, "stroke")), "Tree splits capture interactions automatically",
  "XGBoost", "Full", paste(setdiff(rf_xgb_vars, "stroke"), collapse = ", "), 
    length(setdiff(rf_xgb_vars, "stroke")), "None (implicit)", 0, 
    length(setdiff(rf_xgb_vars, "stroke")), "Boosted trees learn interactions",
  "XGBoost", "Simple", paste(setdiff(rf_xgb_simpl_vars, "stroke"), collapse = ", "), 
    length(setdiff(rf_xgb_simpl_vars, "stroke")), "None (implicit)", 0, 
    length(setdiff(rf_xgb_simpl_vars, "stroke")), "Boosted trees learn interactions",
  "SVM", "Full", paste(setdiff(rf_xgb_vars, "stroke"), collapse = ", "), 
    length(setdiff(rf_xgb_vars, "stroke")), "None (kernel)", 0, 
    length(setdiff(rf_xgb_vars, "stroke")), "Kernel function captures non-linear relationships",
  "SVM", "Simple", paste(setdiff(rf_xgb_simpl_vars, "stroke"), collapse = ", "), 
    length(setdiff(rf_xgb_simpl_vars, "stroke")), "None (kernel)", 0, 
    length(setdiff(rf_xgb_simpl_vars, "stroke")), "Kernel function captures non-linear relationships",
  "MLP", "Full", paste(setdiff(rf_xgb_vars, "stroke"), collapse = ", "), 
    length(setdiff(rf_xgb_vars, "stroke")), "None (hidden layers)", 0, 
    length(setdiff(rf_xgb_vars, "stroke")), "Neural network hidden layers learn interactions",
  "MLP", "Simple", paste(setdiff(rf_xgb_simpl_vars, "stroke"), collapse = ", "), 
    length(setdiff(rf_xgb_simpl_vars, "stroke")), "None (hidden layers)", 0, 
    length(setdiff(rf_xgb_simpl_vars, "stroke")), "Neural network hidden layers learn interactions",
  "Bayesian", "Full", paste(setdiff(rf_xgb_vars, "stroke"), collapse = ", "), 
    length(setdiff(rf_xgb_vars, "stroke")), paste(bayes_full_interactions, collapse = ", "), 
    length(bayes_full_interactions), length(setdiff(rf_xgb_vars, "stroke")) + length(bayes_full_interactions), 
    "Formula-specified age interactions with vessel grades",
  "Bayesian", "Simple", paste(setdiff(rf_xgb_simpl_vars, "stroke"), collapse = ", "), 
    length(setdiff(rf_xgb_simpl_vars, "stroke")), paste(bayes_simpl_interactions, collapse = ", "), 
    length(bayes_simpl_interactions), length(setdiff(rf_xgb_simpl_vars, "stroke")) + length(bayes_simpl_interactions), 
    "Formula-specified age interactions"
)
#- 4.4.4: Print narrative summary
cat("\n=== Variables and Interactions Used by Each ML Method ===\n\n")
cat("Dataset Definitions:\n")
cat(sprintf("• ml_modeling_data: %d variables (%s)\n", 
            length(ml_modeling_data_vars), paste(ml_modeling_data_vars, collapse = ", ")))
cat(sprintf("• rf_xgb (Full): %d variables after removing tot_vert_inj, max_carotid, tot_carotid_inj, max_vert, ID\n", 
            length(rf_xgb_vars)))
cat(sprintf("  Variables: %s\n", paste(rf_xgb_vars, collapse = ", ")))
cat(sprintf("• rf_xgb_simpl (Simple): %d variables (stroke, ASA, sexM, age, max_carotid, max_vert)\n\n", 
            length(rf_xgb_simpl_vars)))
cat("Method-Specific Details:\n\n")
cat("• LASSO (Simple dataset only):\n")
cat(sprintf("  - Base variables: %s\n", paste(setdiff(rf_xgb_simpl_vars, "stroke"), collapse = ", ")))
cat(sprintf("  - Interaction terms: %s\n", paste(lasso_interactions, collapse = ", ")))
cat(sprintf("  - Total features: %d\n\n", length(setdiff(rf_xgb_simpl_vars, "stroke")) + length(lasso_interactions)))
cat("• GAM (Simple dataset only):\n")
cat(sprintf("  - Base variables: %s\n", paste(setdiff(rf_xgb_simpl_vars, "stroke"), collapse = ", ")))
cat(sprintf("  - Interaction terms: %s\n", paste(gam_interactions, collapse = ", ")))
cat(sprintf("  - Total features: %d\n\n", length(setdiff(rf_xgb_simpl_vars, "stroke")) + length(gam_interactions)))
cat("• Random Forest (Both datasets):\n")
cat(sprintf("  - Full dataset: %d variables (no explicit interactions; tree splits capture them)\n", length(setdiff(rf_xgb_vars, "stroke"))))
cat(sprintf("  - Simple dataset: %d variables (no explicit interactions)\n\n", length(setdiff(rf_xgb_simpl_vars, "stroke"))))
cat("• XGBoost (Both datasets):\n")
cat(sprintf("  - Full dataset: %d variables (no explicit interactions; boosted trees learn them)\n", length(setdiff(rf_xgb_vars, "stroke"))))
cat(sprintf("  - Simple dataset: %d variables (no explicit interactions)\n\n", length(setdiff(rf_xgb_simpl_vars, "stroke"))))
cat("• SVM (Both datasets):\n")
cat(sprintf("  - Full dataset: %d variables (kernel function captures non-linear relationships)\n", length(setdiff(rf_xgb_vars, "stroke"))))
cat(sprintf("  - Simple dataset: %d variables (kernel function captures relationships)\n\n", length(setdiff(rf_xgb_simpl_vars, "stroke"))))
cat("• MLP (Both datasets):\n")
cat(sprintf("  - Full dataset: %d variables (hidden layers learn interactions)\n", length(setdiff(rf_xgb_vars, "stroke"))))
cat(sprintf("  - Simple dataset: %d variables (hidden layers learn interactions)\n\n", length(setdiff(rf_xgb_simpl_vars, "stroke"))))
cat("• Bayesian Logistic Regression (Both datasets):\n")
cat(sprintf("  - Full dataset: %d base + %d interactions = %d total features\n", 
            length(setdiff(rf_xgb_vars, "stroke")), length(bayes_full_interactions), 
            length(setdiff(rf_xgb_vars, "stroke")) + length(bayes_full_interactions)))
cat(sprintf("    Interactions: %s\n", paste(bayes_full_interactions, collapse = ", ")))
cat(sprintf("  - Simple dataset: %d base + %d interactions = %d total features\n", 
            length(setdiff(rf_xgb_simpl_vars, "stroke")), length(bayes_simpl_interactions), 
            length(setdiff(rf_xgb_simpl_vars, "stroke")) + length(bayes_simpl_interactions)))
cat(sprintf("    Interactions: %s\n\n", paste(bayes_simpl_interactions, collapse = ", ")))
cat("Key Findings:\n")
cat("• Variables NEVER tested in ML: BLC, BLV (bilateral indicators), GCS, ISS\n")
cat("• Variables removed before ML: tot_vert_inj, tot_carotid_inj\n")
cat("• Methods with explicit interactions: LASSO, GAM, Bayesian\n")
cat("• Methods with implicit interactions: Random Forest, XGBoost, SVM, MLP\n\n")
#- 4.4.5: Print detailed tibble
cat("=== Detailed Variable Documentation Table ===\n\n")
print(variable_documentation, n = Inf) 