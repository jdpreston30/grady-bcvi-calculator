#* LASSO Function
run_lasso_analysis <- function(data, model_label = "LASSO", use_weights = TRUE) {
  # Prepare outcome variables
  y_factor <- factor(data$stroke, levels = c("N", "Y")) # For CV splitting
  y_numeric <- as.numeric(y_factor == "Y") # For model fitting
  # Prepare predictors
  X <- data %>%
    select(-stroke) %>%
    mutate(
      age_ASA = age * as.numeric(as.factor(ASA)),
      age_max_vert = age * max_vert,
      age_max_carotid = age * max_carotid
    ) %>%
    mutate(across(where(is.factor), ~ as.numeric(as.factor(.)))) %>%
    as.matrix()

  # Class weights
  weights <- if (use_weights) {
    ifelse(y_factor == "Y", sum(y_factor == "N") / sum(y_factor == "Y"), 1)
  } else {
    rep(1, length(y_factor))
  }
  # CV Repeats
  lasso_repeat_results <- purrr::map(1:10, function(i) {
    set.seed(2025 + i)
    foldid <- caret::createFolds(y_factor, k = 10, list = FALSE)

    lasso_fit <- suppressMessages(suppressWarnings(glmnet::cv.glmnet(
      x = X, y = y_numeric,
      family = "binomial",
      alpha = 1,
      type.measure = "auc",
      foldid = foldid,
      weights = weights,
      keep = TRUE  # Store held-out fold predictions in fit.preval
    )))

    # --- In-sample predictions (for preds_cv / ROC / Platt — unchanged) ---
    lasso_prob <- predict(lasso_fit, newx = X, s = "lambda.min", type = "response")
    lasso_pred <- ifelse(lasso_prob > 0.5, "Y", "N")

    # --- True held-out fold predictions (for performance metrics only) ---
    lambda_idx <- which(lasso_fit$lambda == lasso_fit$lambda.min)
    cv_linear_pred <- lasso_fit$fit.preval[, lambda_idx]
    cv_prob <- 1 / (1 + exp(-cv_linear_pred))  # inverse logit

    # Per-fold held-out metrics (1:1 with caret-based methods)
    fold_metrics <- purrr::map_dfr(sort(unique(foldid)), function(k) {
      fold_idx <- which(foldid == k)
      fold_truth <- y_factor[fold_idx]
      fold_prob <- cv_prob[fold_idx]
      fold_pred <- factor(ifelse(fold_prob > 0.5, "Y", "N"), levels = c("N", "Y"))
      suppressMessages(suppressWarnings({
        confmat <- caret::confusionMatrix(fold_pred, fold_truth, positive = "Y")
        auc_val <- tryCatch(as.numeric(pROC::auc(fold_truth, fold_prob)),
                            error = function(e) NA_real_)
      }))
      tibble::tibble(
        rep = i, fold = k,
        AUC = auc_val,
        Sensitivity = confmat$byClass["Sensitivity"],
        Specificity = confmat$byClass["Specificity"]
      )
    })

    # Return both: fold_metrics for performance, full-dataset probs for preds_cv
    list(
      fold_metrics = fold_metrics,
      preds = tibble::tibble(
        rep = i,
        truth = as.character(y_factor),
        prob = as.vector(lasso_prob)  # in-sample — intentionally kept for ROC/Platt
      )
    )
  })

  # Separate fold-level metrics from prediction data
  all_fold_metrics <- purrr::map_dfr(lasso_repeat_results, "fold_metrics")
  all_preds <- purrr::map_dfr(lasso_repeat_results, "preds")
  # Final model fit
  final_foldid <- caret::createFolds(y_factor, k = 10, list = FALSE)
  lasso_fit_final <- suppressMessages(suppressWarnings(glmnet::cv.glmnet(
    x = X, y = y_numeric,
    family = "binomial",
    alpha = 1,
    type.measure = "auc",
    foldid = final_foldid,
    weights = weights
  )))
  lasso_probs_train <- predict(lasso_fit_final, newx = X, s = "lambda.min", type = "response")
  lasso_preds_train <- ifelse(lasso_probs_train > 0.5, "Y", "N")
  suppressMessages(suppressWarnings({
    confmat_train <- caret::confusionMatrix(factor(lasso_preds_train, levels = c("N", "Y")), y_factor, positive = "Y")
    auc_train <- pROC::auc(y_factor, as.vector(lasso_probs_train))
  }))
  selected_vars <- coef(lasso_fit_final, s = "lambda.min") %>%
    as.matrix() %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Variable") %>%
    setNames(c("Variable", "Coefficient")) %>%
    filter(Coefficient != 0)
  return(list(
    summary_train = tibble::tibble(
      Model = paste(
        model_label, "(Train Prediction, Optimistic)",
        if (use_weights) "+ weights" else ""
      ),
      AUC = as.numeric(auc_train),
      Sensitivity = confmat_train$byClass["Sensitivity"],
      Specificity = confmat_train$byClass["Specificity"]
    ),
    summary_cv = all_fold_metrics %>%
      summarise(
        Model = paste(
          model_label, "(CV Prediction, Realistic)",
          if (use_weights) "+ weights" else ""
        ),
        AUC = mean(AUC, na.rm = TRUE),
        Sensitivity = mean(Sensitivity, na.rm = TRUE),
        Specificity = mean(Specificity, na.rm = TRUE)
      ),
    metrics_cv = all_fold_metrics %>% select(AUC, Sensitivity, Specificity),  # 100 fold-level obs (1:1 with caret methods)
    selected_variables = selected_vars,
    preds_cv = all_preds %>% select(truth, prob),  # In-sample predictions — unchanged for ROC/Platt
    lasso_fit_final = lasso_fit_final
  ))
}

#* LASSO Wrapper
run_lasso_all_variants <- function(data) {
  results <- list(
    weighted = run_lasso_analysis(data, model_label = "LASSO", use_weights = TRUE),
    unweighted = run_lasso_analysis(data, model_label = "LASSO", use_weights = FALSE)
  )
  attr(results, "Dataset") <- "Simple"
  return(results)
}