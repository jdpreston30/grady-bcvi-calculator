#* Utility: Document Variables and Interactions Used by ML Methods
document_model_variables <- function(ml_modeling_data) {
  # Define variable structures for each dataset
  ml_modeling_data_vars <- names(ml_modeling_data)
  rf_xgb_vars <- ml_modeling_data %>% select(-c(tot_vert_inj, max_carotid, tot_carotid_inj, max_vert), -ID) %>% names()
  rf_xgb_simpl_vars <- ml_modeling_data %>% select(stroke, ASA, sexM, age, max_carotid, max_vert) %>% names()
  
  # Define interaction terms for each method
  lasso_interactions <- c("age_ASA", "age_max_vert", "age_max_carotid")
  gam_interactions <- c("age_ASA", "age_max_vert", "age_max_carotid")
  bayes_full_interactions <- c("ASA:age", "age:Max_LC", "age:Max_RC", "age:Max_LV", "age:Max_RV")
  bayes_simpl_interactions <- c("ASA:age", "max_vert:age", "max_carotid:age")
  
  # Create detailed tibble of variables by method
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
  
  # Print narrative summary
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
  cat("=== Detailed Variable Documentation Table ===\n\n")
  print(variable_documentation, n = Inf)
  
  # Return the tibble invisibly for further use if needed
  invisible(variable_documentation)
}
