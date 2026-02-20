#* LASSO CV Comparison: In-Sample vs True Held-Out Fold Predictions
#* ================================================================
#* Purpose: Compare current LASSO performance metrics (in-sample predictions
#*          on the full dataset) against properly extracted held-out cross-
#*          validation predictions using cv.glmnet's fit.preval.
#*
#* The key difference: the original pipeline calls
#*     predict(lasso_fit, newx = X, s = "lambda.min")
#* which predicts on the FULL dataset (in-sample). The corrected version
#* extracts the actual held-out fold predictions that cv.glmnet already
#* computed internally, by passing keep = TRUE and reading fit.preval.
#*
#* NOTE: This script does NOT modify any existing pipeline code or saved results.
#* ================================================================

# --- 0: Setup ---
if (!grepl("Publication$", getwd())) {
  if (dir.exists("Publication")) setwd("Publication")
}

suppressPackageStartupMessages({
  library(glmnet)
  library(caret)
  library(pROC)
  library(dplyr)
  library(tibble)
  library(purrr)
})

# --- 1: Load data ---
if (!exists("ml_modeling_data")) {
  message("ml_modeling_data not found in environment. Sourcing scripts 00-03...")
  source("scripts/00_setup.r")
  source("scripts/01_import_and_preprocess.r")
  source("scripts/02_descriptive_statistics.r")
  source("scripts/03_trad_linear_modeling.r")
  message("✓ Data loaded successfully.")
}

# --- 2: Prepare LASSO inputs (identical to original M01_LASSO_pipeline.r) ---
rf_xgb_simpl <- ml_modeling_data %>%
  select(stroke, ASA, sexM, age, max_carotid, max_vert)

data <- rf_xgb_simpl
y_factor <- factor(data$stroke, levels = c("N", "Y"))
y_numeric <- as.numeric(y_factor == "Y")

X <- data %>%
  select(-stroke) %>%
  mutate(
    age_ASA = age * as.numeric(as.factor(ASA)),
    age_max_vert = age * max_vert,
    age_max_carotid = age * max_carotid
  ) %>%
  mutate(across(where(is.factor), ~ as.numeric(as.factor(.)))) %>%
  as.matrix()

weights <- ifelse(y_factor == "Y", sum(y_factor == "N") / sum(y_factor == "Y"), 1)

cat("\nDataset: n =", nrow(X), "| stroke events =", sum(y_numeric), 
    "| features =", ncol(X), "\n")

# --- 3: Run BOTH approaches side-by-side (10 repeats, same seeds) ---
cat("\n", strrep("=", 70), "\n", sep = "")
cat("  LASSO CV COMPARISON: In-Sample vs True Held-Out Predictions\n")
cat(strrep("=", 70), "\n\n")
cat("Running 10 repeats of 10-fold CV (identical seeds to original)...\n\n")

results_original <- list()
results_corrected <- list()     # per-fold metrics (1:1 with other methods)
results_corrected_pooled <- list()  # pooled per-repeat (previous version, for reference)

for (i in 1:10) {
  set.seed(2025 + i)
  foldid <- caret::createFolds(y_factor, k = 10, list = FALSE)
  
  # Fit cv.glmnet with keep=TRUE to store held-out predictions
  lasso_fit <- suppressMessages(suppressWarnings(glmnet::cv.glmnet(
    x = X, y = y_numeric,
    family = "binomial",
    alpha = 1,
    type.measure = "auc",
    foldid = foldid,
    weights = weights,
    keep = TRUE  # <-- stores cross-validated predictions in fit.preval
  )))
  
  # ====================================================================
  # ORIGINAL APPROACH: predict on full dataset (current pipeline)
  # ====================================================================
  orig_prob <- as.vector(predict(lasso_fit, newx = X, s = "lambda.min", type = "response"))
  orig_pred <- ifelse(orig_prob > 0.5, "Y", "N")
  suppressMessages(suppressWarnings({
    orig_confmat <- caret::confusionMatrix(
      factor(orig_pred, levels = c("N", "Y")), y_factor, positive = "Y")
    orig_auc <- as.numeric(pROC::auc(y_factor, orig_prob))
  }))
  results_original[[i]] <- tibble(
    rep = i, AUC = orig_auc,
    Sensitivity = orig_confmat$byClass["Sensitivity"],
    Specificity = orig_confmat$byClass["Specificity"]
  )
  
  # ====================================================================
  # CORRECTED APPROACH: extract held-out fold predictions from fit.preval
  # Compute metrics PER FOLD (identical to how GAM, MLP, GBM, etc. do it)
  # ====================================================================
  lambda_idx <- which(lasso_fit$lambda == lasso_fit$lambda.min)
  cv_linear_pred <- lasso_fit$fit.preval[, lambda_idx]
  cv_prob <- 1 / (1 + exp(-cv_linear_pred))  # inverse logit to probability
  
  # Per-fold metrics (this is what makes it 1:1 with the other methods)
  for (k in sort(unique(foldid))) {
    fold_idx <- which(foldid == k)
    fold_truth <- y_factor[fold_idx]
    fold_prob <- cv_prob[fold_idx]
    fold_pred <- factor(ifelse(fold_prob > 0.5, "Y", "N"), levels = c("N", "Y"))
    
    suppressMessages(suppressWarnings({
      fold_confmat <- caret::confusionMatrix(fold_pred, fold_truth, positive = "Y")
      fold_auc <- as.numeric(pROC::auc(fold_truth, fold_prob))
    }))
    
    results_corrected[[length(results_corrected) + 1]] <- tibble(
      rep = i, fold = k,
      AUC = fold_auc,
      Sensitivity = fold_confmat$byClass["Sensitivity"],
      Specificity = fold_confmat$byClass["Specificity"]
    )
  }
  
  # Also compute pooled per-repeat (for reference comparison with previous run)
  cv_pred_all <- ifelse(cv_prob > 0.5, "Y", "N")
  suppressMessages(suppressWarnings({
    cv_confmat <- caret::confusionMatrix(
      factor(cv_pred_all, levels = c("N", "Y")), y_factor, positive = "Y")
    cv_auc <- as.numeric(pROC::auc(y_factor, cv_prob))
  }))
  results_corrected_pooled[[i]] <- tibble(
    rep = i, AUC = cv_auc,
    Sensitivity = cv_confmat$byClass["Sensitivity"],
    Specificity = cv_confmat$byClass["Specificity"]
  )
  
  cat(sprintf("  Rep %2d | Original (in-sample): AUC=%.3f Sens=%.0f%% Spec=%.0f%% | Corrected (pooled): AUC=%.3f Sens=%.0f%% Spec=%.0f%%\n",
              i, orig_auc, orig_confmat$byClass["Sensitivity"]*100, orig_confmat$byClass["Specificity"]*100,
              cv_auc, cv_confmat$byClass["Sensitivity"]*100, cv_confmat$byClass["Specificity"]*100))
}

# --- 4: Summarize and compare ---
orig_df <- bind_rows(results_original)        # 10 rows (one per repeat, pooled)
corr_df <- bind_rows(results_corrected)        # 100 rows (10 folds × 10 repeats) — 1:1 with other methods
corr_pooled_df <- bind_rows(results_corrected_pooled)  # 10 rows (pooled per repeat, for reference)

cat("\n", strrep("=", 70), "\n", sep = "")
cat("  Per-fold corrected results: ", nrow(corr_df), " observations",
    " (10 folds × 10 repeats)\n", sep = "")
cat("  This matches the structure used by GAM, MLP, RF, GBM, SVM, Bayes\n")
cat(strrep("=", 70), "\n\n")

# --- Original metrics (10 repeat-level observations, in-sample) ---
orig_summary <- orig_df %>% summarise(
  AUC = mean(AUC), Sensitivity = mean(Sensitivity), Specificity = mean(Specificity),
  Youdens_J = mean(Sensitivity) + mean(Specificity) - 1
)
orig_ci <- orig_df %>% summarise(
  AUC_lo = quantile(AUC, 0.025), AUC_hi = quantile(AUC, 0.975),
  Sens_lo = quantile(Sensitivity, 0.025), Sens_hi = quantile(Sensitivity, 0.975),
  Spec_lo = quantile(Specificity, 0.025), Spec_hi = quantile(Specificity, 0.975)
)

# --- Corrected per-fold metrics (100 observations, 1:1 with other methods) ---
corr_summary <- corr_df %>% summarise(
  AUC = mean(AUC, na.rm = TRUE),
  Sensitivity = mean(Sensitivity, na.rm = TRUE),
  Specificity = mean(Specificity, na.rm = TRUE),
  Youdens_J = mean(Sensitivity, na.rm = TRUE) + mean(Specificity, na.rm = TRUE) - 1
)
corr_ci <- corr_df %>% summarise(
  AUC_lo = quantile(AUC, 0.025, na.rm = TRUE), AUC_hi = quantile(AUC, 0.975, na.rm = TRUE),
  Sens_lo = quantile(Sensitivity, 0.025, na.rm = TRUE), Sens_hi = quantile(Sensitivity, 0.975, na.rm = TRUE),
  Spec_lo = quantile(Specificity, 0.025, na.rm = TRUE), Spec_hi = quantile(Specificity, 0.975, na.rm = TRUE)
)

# --- Corrected pooled (for reference / comparison with previous run) ---
corr_pooled_summary <- corr_pooled_df %>% summarise(
  AUC = mean(AUC), Sensitivity = mean(Sensitivity), Specificity = mean(Specificity),
  Youdens_J = mean(Sensitivity) + mean(Specificity) - 1
)

cat("ORIGINAL (in-sample predictions — current pipeline, 10 repeats):\n")
cat(sprintf("  AUC:         %.2f [%.2f\u2013%.2f]\n", orig_summary$AUC, orig_ci$AUC_lo, orig_ci$AUC_hi))
cat(sprintf("  Sensitivity: %d%% [%d%%\u2013%d%%]\n", round(orig_summary$Sensitivity*100), round(orig_ci$Sens_lo*100), round(orig_ci$Sens_hi*100)))
cat(sprintf("  Specificity: %d%% [%d%%\u2013%d%%]\n", round(orig_summary$Specificity*100), round(orig_ci$Spec_lo*100), round(orig_ci$Spec_hi*100)))
cat(sprintf("  Youden's J:  %.2f\n", orig_summary$Youdens_J))

cat("\nCORRECTED PER-FOLD (held-out predictions, 100 fold-level obs — 1:1 with other methods):\n")
cat(sprintf("  AUC:         %.2f [%.2f\u2013%.2f]\n", corr_summary$AUC, corr_ci$AUC_lo, corr_ci$AUC_hi))
cat(sprintf("  Sensitivity: %d%% [%d%%\u2013%d%%]\n", round(corr_summary$Sensitivity*100), round(corr_ci$Sens_lo*100), round(corr_ci$Sens_hi*100)))
cat(sprintf("  Specificity: %d%% [%d%%\u2013%d%%]\n", round(corr_summary$Specificity*100), round(corr_ci$Spec_lo*100), round(corr_ci$Spec_hi*100)))
cat(sprintf("  Youden's J:  %.2f\n", corr_summary$Youdens_J))

cat("\nCORRECTED POOLED (held-out predictions, 10 repeat-level obs — for reference):\n")
cat(sprintf("  AUC:         %.2f   Sens: %d%%   Spec: %d%%   J: %.2f\n",
    corr_pooled_summary$AUC, round(corr_pooled_summary$Sensitivity*100),
    round(corr_pooled_summary$Specificity*100), corr_pooled_summary$Youdens_J))

cat("\n", strrep("-", 70), "\n", sep = "")
cat("DIFFERENCE (Original - Corrected Per-Fold):\n")
cat(sprintf("  \u0394 AUC:         %+.4f\n", orig_summary$AUC - corr_summary$AUC))
cat(sprintf("  \u0394 Sensitivity: %+.1f pp\n", (orig_summary$Sensitivity - corr_summary$Sensitivity)*100))
cat(sprintf("  \u0394 Specificity: %+.1f pp\n", (orig_summary$Specificity - corr_summary$Specificity)*100))
cat(sprintf("  \u0394 Youden's J:  %+.4f\n", orig_summary$Youdens_J - corr_summary$Youdens_J))

# --- Check for NA folds (small folds with 0 events) ---
na_count <- sum(is.na(corr_df$AUC))
if (na_count > 0) {
  cat(sprintf("\n  \u26A0\uFE0F  %d of %d folds had NA AUC (likely 0 events in fold). These were excluded from means.\n",
      na_count, nrow(corr_df)))
}

cat(strrep("-", 70), "\n")
cat("\nINTERPRETATION:\n")
auc_diff <- abs(orig_summary$AUC - corr_summary$AUC)
sens_diff <- abs(orig_summary$Sensitivity - corr_summary$Sensitivity) * 100
spec_diff <- abs(orig_summary$Specificity - corr_summary$Specificity) * 100

if (auc_diff < 0.02 & sens_diff < 5 & spec_diff < 5) {
  cat("\u2705 Differences are SMALL. The LASSO regularization effectively\n")
  cat("   prevented overfitting. The corrected per-fold metrics are now\n")
  cat("   directly comparable to all other methods in the manuscript.\n")
} else if (auc_diff < 0.05 & sens_diff < 10 & spec_diff < 10) {
  cat("\u26A0\uFE0F  Differences are MODERATE. Consider updating manuscript metrics\n")
  cat("   to the corrected values. The corrected per-fold metrics are now\n")
  cat("   directly comparable to all other methods in the manuscript.\n")
} else {
  cat("\u274C Differences are LARGE. The in-sample metrics were substantially\n")
  cat("   optimistic. A different strategy for the reviewer response is\n")
  cat("   recommended.\n")
}

cat("\n", strrep("=", 70), "\n\n", sep = "")
