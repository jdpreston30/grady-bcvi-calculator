#* Bayes Function
run_bayeslog_model <- function(
    dataset,
    formula,
    model_label = "Bayesian",
    seed = 2025,
    sampling_method = NULL,
    use_weights = FALSE,
    response = "stroke",
    repeats = 10,
    folds = 10) {
  set.seed(seed)

  if (!is.null(sampling_method) && sampling_method == "down") {
    dataset <- dataset %>%
      group_by(!!sym(response)) %>%
      sample_n(min(table(dataset[[response]]))) %>%
      ungroup()
  }

  dataset[[response]] <- factor(dataset[[response]], levels = c("N", "Y"))
  y <- dataset[[response]]

  drop_low_variance <- function(dataset, response_col, threshold = 0.95) {
    keep_cols <- sapply(dataset, function(col) {
      if (is.numeric(col) || is.factor(col)) {
        max(prop.table(table(col))) < threshold
      } else {
        TRUE
      }
    })
    keep_cols[names(keep_cols) == response_col] <- TRUE
    dataset[, keep_cols]
  }

  dataset <- drop_low_variance(dataset, response)

  dataset$obs_weights <- if (use_weights) {
    ifelse(dataset[[response]] == "Y", 1, sum(dataset[[response]] == "Y") / sum(dataset[[response]] == "N"))
  } else {
    rep(1, nrow(dataset))
  }

  environment(formula) <- environment()

  # Train final model on all data
  bayes_fit <- suppressWarnings(
    arm::bayesglm(
      formula = formula,
      data = dataset,
      family = binomial(),
      weights = dataset[["obs_weights"]]
    )
  )

  prob_train <- predict(bayes_fit, type = "response")
  pred_train <- ifelse(prob_train > 0.5, "Y", "N")
  confmat_train <- caret::confusionMatrix(factor(pred_train, levels = c("N", "Y")), y, positive = "Y")
  auc_train <- pROC::auc(y, prob_train)

  # --- Cross-validation with fold-level averaging ---
  fold_metrics <- list()
  preds_cv_all <- list()

  for (i in seq_len(repeats)) {
    set.seed(seed + i)
    folds_list <- caret::createFolds(y, k = folds, returnTrain = FALSE)

    for (fold in seq_len(folds)) {
      test_idx <- folds_list[[fold]]
      train_data <- dataset[-test_idx, ]
      test_data <- dataset[test_idx, ]

      local_formula <- formula
      environment(local_formula) <- environment()

      fold_fit <- suppressWarnings(
        arm::bayesglm(
          formula = local_formula,
          data = train_data,
          family = binomial(),
          weights = train_data[["obs_weights"]]
        )
      )

      prob <- predict(fold_fit, newdata = test_data, type = "response")
      truth <- factor(test_data[[response]], levels = c("N", "Y"))
      pred <- factor(ifelse(prob > 0.5, "Y", "N"), levels = c("N", "Y"))

      confmat <- caret::confusionMatrix(pred, truth, positive = "Y")
      auc_val <- as.numeric(pROC::auc(truth, prob))

      fold_metrics[[length(fold_metrics) + 1]] <- tibble(
        AUC = auc_val,
        Sensitivity = confmat$byClass["Sensitivity"],
        Specificity = confmat$byClass["Specificity"]
      )

      preds_cv_all[[length(preds_cv_all) + 1]] <- tibble(
        truth = truth,
        prob = prob
      )
    }
  }

  metrics_cv <- bind_rows(fold_metrics)
  preds_cv <- bind_rows(preds_cv_all)

  summary_cv <- metrics_cv %>%
    summarise(
      Model = glue::glue("{model_label} (CV Prediction){if (!is.null(sampling_method)) ', Downsampled' else ''}{if (use_weights) ', Weighted' else ''}"),
      AUC = mean(AUC, na.rm = TRUE),
      Sensitivity = mean(Sensitivity, na.rm = TRUE),
      Specificity = mean(Specificity, na.rm = TRUE)
    )

  return(list(
    summary_train = tibble::tibble(
      Model = glue::glue("{model_label} (Train Prediction){if (!is.null(sampling_method)) ', Downsampled' else ''}{if (use_weights) ', Weighted' else ''}"),
      AUC = as.numeric(auc_train),
      Sensitivity = confmat_train$byClass["Sensitivity"],
      Specificity = confmat_train$byClass["Specificity"]
    ),
    summary_cv = summary_cv,
    preds_cv = preds_cv,
    bayes_fit = bayes_fit
  ))
}

#* Bayes Wrapper
run_bayeslog_all_variants <- function(
    full_dataset, simple_dataset,
    formula_full, formula_simple,
    seed = 2025,
    repeats = 10,
    folds = 10) {
  results <- list(
    full_down = run_bayeslog_model(
      dataset = full_dataset,
      formula = formula_full,
      model_label = "Bayesian Full (Downsample)",
      seed = seed,
      sampling_method = "down",
      use_weights = FALSE,
      repeats = repeats,
      folds = folds
    ),
    full_nodown = run_bayeslog_model(
      dataset = full_dataset,
      formula = formula_full,
      model_label = "Bayesian Full",
      seed = seed,
      sampling_method = NULL,
      use_weights = FALSE,
      repeats = repeats,
      folds = folds
    ),
    simpl_down = run_bayeslog_model(
      dataset = simple_dataset,
      formula = formula_simple,
      model_label = "Bayesian Simplified (Downsample)",
      seed = seed,
      sampling_method = "down",
      use_weights = FALSE,
      repeats = repeats,
      folds = folds
    ),
    simpl_nodown = run_bayeslog_model(
      dataset = simple_dataset,
      formula = formula_simple,
      model_label = "Bayesian Simplified",
      seed = seed,
      sampling_method = NULL,
      use_weights = FALSE,
      repeats = repeats,
      folds = folds
    )
  )

  attr(results$full_down, "Dataset") <- "Complex"
  attr(results$full_nodown, "Dataset") <- "Complex"
  attr(results$simpl_down, "Dataset") <- "Simple"
  attr(results$simpl_nodown, "Dataset") <- "Simple"

  return(results)
}
