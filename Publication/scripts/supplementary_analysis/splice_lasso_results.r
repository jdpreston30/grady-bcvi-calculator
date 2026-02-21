#* Splice LASSO Results into all_model_results.rds
#* 
#* Purpose: Re-run ONLY the LASSO pipeline with the updated function
#*          (which now returns preds_cv_heldout for ROC), then overwrite
#*          just the LASSO entries in all_model_results.rds.
#*
#* Prerequisites: 
#*   - Must have ml_modeling_data in the environment (run scripts 00-03 first,
#*     OR load it from the RDS if you saved it)
#*   - Must be in the Publication/ working directory
#*
#* What this changes:
#*   - LASSO weighted and unweighted entries in all_model_results.rds
#*     (adds preds_cv_heldout field)
#*
#* What this does NOT change:
#*   - Any other model's results
#*   - Platt scaling (still uses preds_cv, which is in-sample)
#*   - Calculator, equations, coefficients
#*   - metrics_cv, summary_cv (identical to previous run with same seeds)

cat("=== Splice LASSO Results ===\n\n")

# --- 1. Load existing results ---
rds_path <- "Outputs/Models/all_model_results.rds"
if (!file.exists(rds_path)) stop("Cannot find ", rds_path, " — are you in Publication/?")
all_model_results <- readRDS(rds_path)
cat("Loaded existing all_model_results.rds\n")

# --- 2. Source the updated LASSO function ---
source("R/modeling_pipelines/M01_LASSO_pipeline.r")
cat("Sourced updated LASSO pipeline (with preds_cv_heldout)\n")

# --- 3. Verify ml_modeling_data exists ---
if (!exists("ml_modeling_data")) {
  stop("ml_modeling_data not found in environment.\n",
       "Either run scripts 00-03 first, or load it manually.")
}

# --- 4. Prepare simple dataset (same as 04_modeling_and_performance.r) ---
rf_xgb_simpl <- ml_modeling_data %>%
  dplyr::select(stroke, ASA, sexM, age, max_carotid, max_vert)

cat("Re-running LASSO (weighted + unweighted)...\n")

# --- 5. Re-run LASSO ---
lasso_all_variants <- run_lasso_all_variants(rf_xgb_simpl)

cat("LASSO re-run complete.\n\n")

# --- 6. Verify new field exists ---
stopifnot("preds_cv_heldout" %in% names(lasso_all_variants$weighted))
stopifnot("preds_cv_heldout" %in% names(lasso_all_variants$unweighted))
cat("✅ preds_cv_heldout present in both variants\n")

# --- 7. Quick sanity check: metrics should match previous run ---
old_lasso_auc <- all_model_results$lasso$weighted$summary_cv$AUC
new_lasso_auc <- lasso_all_variants$weighted$summary_cv$AUC
cat(sprintf("Old weighted AUC: %.4f\n", old_lasso_auc))
cat(sprintf("New weighted AUC: %.4f\n", new_lasso_auc))
if (abs(old_lasso_auc - new_lasso_auc) > 0.01) {
  warning("⚠️  AUC differs by more than 0.01 — seeding may have changed!")
} else {
  cat("✅ AUC matches within tolerance (seed stable)\n")
}

# --- 8. Splice into all_model_results ---
all_model_results$lasso <- lasso_all_variants
cat("\n✅ LASSO results spliced into all_model_results\n")

# --- 9. Save ---
saveRDS(all_model_results, rds_path)
cat("✅ Saved updated all_model_results.rds\n")

# --- 10. Also update the global variable for downstream scripts ---
cat("\nlasso_all_variants is now in your environment.\n")
cat("You can now re-run scripts 05 (ROC) and it will use held-out predictions.\n")
cat("Script 06 (Platt/risk) will still use preds_cv (in-sample) — unchanged.\n")

cat("\n=== Done ===\n")
