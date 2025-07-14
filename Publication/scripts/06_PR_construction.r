#* 7: Construct PR curves for all no tot inj included models
  #+ 7.1 Prepare for PR construction
    #- 7.1.1: Define shared 100-point grid (same as ROC)
      smoothed_points <- seq(0, 1, length.out = 100)
    #- 7.1.2: Function for smoothed PR curves
      get_smoothed_pr <- function(truth, probs, model_name, smoothed_points = seq(0, 1, length.out = 100)) {
        truth_bin <- as.numeric(truth == "Y")
        pr_obj <- PRROC::pr.curve(
          scores.class0 = probs[truth_bin == 1], # Positive class
          scores.class1 = probs[truth_bin == 0], # Negative class
          curve = TRUE
        )
        recall_grid <- smoothed_points
        interp_precision <- approx(x = pr_obj$curve[, 1], y = pr_obj$curve[, 2], xout = recall_grid)$y

        pr_df <- data.frame(
          Recall = recall_grid,
          Precision = interp_precision,
          Model = model_name,
          PRAUC = pr_obj$auc.integral
        )
        return(pr_df)
      }
  #+ 7.3: Compute smoothed PR curves for each model
    #- 7.3.1: SVM
      svm_pr <- get_smoothed_pr(
        truth = factor(svm_all_variants$simpl_downsampled$preds_cv$truth, levels = c("N", "Y")),
        probs = svm_all_variants$simpl_downsampled$preds_cv$prob,
        model_name = "SVM_simpl_downsampled"
      )
    #- 7.3.2: Random Forest
      rf_pr <- get_smoothed_pr(
        truth = factor(rf_all_variants$rf_down_simple$rf_fit$pred$obs, levels = c("N", "Y")),
        probs = rf_all_variants$rf_down_simple$rf_fit$pred$Y,
        model_name = "RF_rf_down_simple"
      )
    #- 7.3.3: XGBoost
      xgb_pr <- get_smoothed_pr(
        truth = xgb_truth, # From ROC section
        probs = xgb_probs,
        model_name = "XGB_simpl_down_only"
      )
    #- 7.3.4: MLP
      mlp_pr <- get_smoothed_pr(
        truth = factor(mlp_all_variants$simpl_down$mlp_fit$pred$obs, levels = c("N", "Y")),
        probs = mlp_all_variants$simpl_down$mlp_fit$pred$Y,
        model_name = "MLP_simpl_down"
      )
    #- 7.3.5: GAM
      gam_pr <- get_smoothed_pr(
        truth = factor(gam_all_variants$simpl_down$gam_fit$pred$obs, levels = c("N", "Y")),
        probs = gam_all_variants$simpl_down$gam_fit$pred$Y,
        model_name = "GAM_simpl_down"
      )
    #- 7.3.6: LASSO
      lasso_pr <- get_smoothed_pr(
        truth = factor(lasso_all_variants$weighted$preds_cv$truth, levels = c("N", "Y")),
        probs = lasso_all_variants$weighted$preds_cv$prob,
        model_name = "LASSO_weighted"
      )
    #- 7.3.7: Bayes
      bayes_pr <- get_smoothed_pr(
        truth = factor(bayes_truth, levels = c("N", "Y")),
        probs = bayes_probs,
        model_name = "Bayes_simpl_down"
      )
  #+ 7.4: Export PR curves
    smoothed_prs <- bind_rows(
      lasso_pr,
      bayes_pr,
      mlp_pr,
      svm_pr,
      gam_pr,
      rf_pr,
      xgb_pr
    ) %>%
      select(Model, Recall, Precision, PRAUC)
    write.csv(smoothed_prs, "smoothed_prs.csv", row.names = FALSE)
