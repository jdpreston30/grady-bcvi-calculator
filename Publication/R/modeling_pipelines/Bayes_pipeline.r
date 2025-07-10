run_bayeslog_model <- function(dataset, formula, model_label = "Bayesian", seed = 2025, sampling_method = NULL, use_weights = FALSE, response = "stroke") {
  set.seed(seed)

  # Downsampling first (before touching response column)
  if (!is.null(sampling_method) && sampling_method == "down") {
    dataset <- dataset %>%
      group_by(!!sym(response)) %>%
      sample_n(min(table(dataset[[response]]))) %>%
      ungroup()
  }

  # Ensure response is a factor with correct level ordering
  dataset[[response]] <- factor(dataset[[response]], levels = c("N", "Y"))
  y <- dataset[[response]]

  # Drop low-variance cols
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

  # Weights
  dataset$obs_weights <- if (use_weights) {
    ifelse(dataset[[response]] == "Y", 1, sum(dataset[[response]] == "Y") / sum(dataset[[response]] == "N"))
  } else {
    rep(1, nrow(dataset))
  }

  # Fix formula environment
  environment(formula) <- environment()

  # Fit Bayesian model
  bayes_fit <- suppressWarnings(
    arm::bayesglm(
      formula = formula,
      data = dataset,
      family = binomial(),
      weights = dataset$obs_weights
    )
  )

  # Evaluate
  prob_train <- predict(bayes_fit, type = "response")
  pred_train <- ifelse(prob_train > 0.5, "Y", "N")
  confmat <- caret::confusionMatrix(factor(pred_train, levels = c("N", "Y")), y, positive = "Y")
  auc_train <- pROC::auc(y, prob_train)

  summary_train <- tibble::tibble(
    Model = glue::glue("{model_label} (Train Prediction){if (!is.null(sampling_method)) ', Downsampled' else ''}{if (use_weights) ', Weighted' else ''}"),
    AUC = as.numeric(auc_train),
    Sensitivity = confmat$byClass["Sensitivity"],
    Specificity = confmat$byClass["Specificity"]
  )

  return(list(
    summary_train = summary_train,
    bayes_fit = bayes_fit
  ))
}