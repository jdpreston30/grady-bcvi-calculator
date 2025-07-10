#* 7: Construct PR curves for all no tot inj included models
  #+ 7.1: Function for smoothed PR curves
    get_smoothed_pr <- function(truth, probs, model_name, smoothed_points = seq(0, 1, length.out = 100)) {
      # Convert to binary (1 = Y, 0 = N)
      truth_bin <- as.numeric(truth == "Y")

      # PRROC wants scores from the positive and negative classes separately
      pr_obj <- PRROC::pr.curve(
        scores.class0 = probs[truth_bin == 1], # Positive class scores
        scores.class1 = probs[truth_bin == 0], # Negative class scores
        curve = TRUE
      )

      # Extract and interpolate to a common Recall grid
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
  #+ 7.2: Compute smoothed PR curves for each model
    #- 7.2.1: Random Forest
      rf_pr <- get_smoothed_pr(
        truth = factor(rf_result_no_tot$rf_fit$pred$obs, levels = c("N", "Y")),
        probs = rf_result_no_tot$rf_fit$pred$Y,
        model_name = "Random Forest"
      )
    #- 7.2.2: XGBoost
      xgb_pr <- get_smoothed_pr(
        truth = factor(rf_xgb_no_tots$stroke, levels = c("N", "Y")),
        probs = xgb_probs,
        model_name = "XGBoost"
      )
    #- 7.2.3: MLP
      mlp_pr <- get_smoothed_pr(
        truth = factor(mlp_result_no_tot$mlp_fit$pred$obs, levels = c("N", "Y")),
        probs = mlp_result_no_tot$mlp_fit$pred$Y,
        model_name = "MLP"
      )
    #- 7.2.4: SVM
      svm_pr <- get_smoothed_pr(
        truth = factor(rf_xgb_no_tots$stroke, levels = c("N", "Y")),
        probs = attr(predict(
          svm_result_no_tot$model,
          rf_xgb_no_tots %>%
            select(-stroke) %>%
            mutate(across(where(is.factor), ~ as.numeric(as.factor(.)))) %>%
            as.data.frame(),
          probability = TRUE
        ), "probabilities")[, "Y"],
        model_name = "SVM"
      )
    #- 7.2.5: GAM
      gam_pr <- get_smoothed_pr(
        truth = factor(gam_result_no_tot$gam_fit$pred$obs, levels = c("N", "Y")),
        probs = gam_result_no_tot$gam_fit$pred$Y,
        model_name = "GAM"
      )
    #- 7.2.6: LASSO
      lasso_pr <- get_smoothed_pr(
        truth = factor(lasso_result_no_tot$preds_cv$truth, levels = c("N", "Y")),
        probs = lasso_result_no_tot$preds_cv$prob,
        model_name = "LASSO"
      )
  #+ 7.3: Export PR curves for Prism graphing
    smoothed_prs <- bind_rows(
      lasso_pr,
      rf_pr,
      mlp_pr,
      gam_pr,
      xgb_pr,
      svm_pr,
    ) %>%
      select(Model, Recall, Precision, PRAUC)
    write.csv(smoothed_prs, "smoothed_prs.csv", row.names = FALSE)
