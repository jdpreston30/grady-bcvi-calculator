#* 6: Construct ROC curves
  #+ 6.0: Load selected models
    selected_models <- list(
      LASSO = "weighted",
      RF    = "rf_down_full",
      XGB   = "simpl_down_only",
      SVM   = "full_downsampled",
      GAM   = "simpl_down",
      MLP   = "simpl_down",
      Bayes = "simpl_down"
    )
  #+ 6.1: Prepare for ROC construction
    #- 6.1.1: Define common 100-point grid for smoothed ROC
      smoothed_points <- seq(0, 1, length.out = 100)
    #- 6.1.2: Helper function to compute smoothed ROC curve
      get_smoothed_roc <- function(truth, probs, model_name, smoothed_points = seq(0, 1, length.out = 100)) {
        roc_curve <- pROC::roc(truth, probs, levels = c("N", "Y"), direction = "<")
        smoothed_roc <- data.frame(
          FPR = smoothed_points,
          TPR = approx(1 - roc_curve$specificities, roc_curve$sensitivities, xout = smoothed_points)$y
        )
        smoothed_roc$Model <- model_name
        smoothed_roc$AUC <- as.numeric(pROC::auc(roc_curve))
        return(smoothed_roc)
      }
  #+ 6.2: Compute smoothed ROC for each model
# Assumes the following are already defined:
# rf_xgb, rf_xgb_simpl, get_smoothed_roc()

# --- 6.2.1: SVM (already done) ---
svm_roc_df <- get_smoothed_roc(
  truth = svm_all_variants$preds_cv$truth,
  probs = svm_all_variants$preds_cv$prob,
  model_name = "SVM_downsampled"
)

# --- 6.2.2: Random Forest ---
rf_roc <- get_smoothed_roc(
  truth = factor(rf_all_variants$rf_down_full$rf_fit$pred$obs, levels = c("N", "Y")),
  probs = rf_all_variants$rf_down_full$rf_fit$pred$Y,
  model_name = "RF_downsampled_full"
)

# --- 6.2.3: XGBoost ---
xgb_matrix <- xgboost::xgb.DMatrix(
  data = rf_xgb_simpl %>%
    select(-stroke) %>%
    mutate(across(where(is.factor), ~ as.numeric(as.factor(.)))) %>%
    as.matrix()
)
xgb_probs <- predict(xgb_all_variants$simpl_down_only$final_model, xgb_matrix)
xgb_truth <- factor(rf_xgb_simpl$stroke, levels = c("N", "Y"))

xgb_roc <- get_smoothed_roc(
  truth = xgb_truth,
  probs = xgb_probs,
  model_name = "XGB_simpl_down_only"
)

# --- 6.2.4: MLP ---
mlp_roc <- get_smoothed_roc(
  truth = factor(mlp_all_variants$simpl_down$mlp_fit$pred$obs, levels = c("N", "Y")),
  probs = mlp_all_variants$simpl_down$mlp_fit$pred$Y,
  model_name = "MLP_simpl_down"
)

# --- 6.2.5: GAM ---
gam_roc <- get_smoothed_roc(
  truth = factor(gam_all_variants$simpl_down$gam_fit$pred$obs, levels = c("N", "Y")),
  probs = gam_all_variants$simpl_down$gam_fit$pred$Y,
  model_name = "GAM_simpl_down"
)

# --- 6.2.6: LASSO ---
lasso_roc <- get_smoothed_roc(
  truth = factor(lasso_all_variants$weighted$preds_cv$truth, levels = c("N", "Y")),
  probs = lasso_all_variants$weighted$preds_cv$prob,
  model_name = "LASSO_weighted"
)

# --- 6.2.7: Bayes ---
# # Reconstruct data used in simpl_down variant (i.e. downsampled simple dataset)
# bayes_data <- rf_xgb_simpl %>%
#   group_by(stroke) %>%
#   sample_n(min(table(stroke))) %>%
#   ungroup()
# bayes_data$stroke <- factor(bayes_data$stroke, levels = c("N", "Y"))

# # Fix formula environment so predict() works
# environment(bayes_all_variants$simpl_down$bayes_fit$formula) <- environment()

# # Predict on the training data
# bayes_preds <- predict(bayes_all_variants$simpl_down$bayes_fit, newdata = bayes_data, type = "response")

# # Construct ROC
# bayes_roc <- get_smoothed_roc(
#   truth = bayes_data$stroke,
#   probs = bayes_preds,
#   model_name = "Bayes_simpl_down (Train)"
# )
# --- 6.3: Combine and write ---
smoothed_rocs <- bind_rows(
  svm_roc_df,
  rf_roc,
  xgb_roc,
  mlp_roc,
  gam_roc,
  lasso_roc
  # bayes_roc
) %>%
  select(Model, FPR, TPR, AUC)

write.csv(smoothed_rocs, "smoothed_rocs.csv", row.names = FALSE)
