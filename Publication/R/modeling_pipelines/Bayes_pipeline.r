run_bayeslog_model <- function(dataset, formula, model_label = "Bayesian", seed = 2025, sampling_method = NULL, use_weights = FALSE, response = "stroke") {
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

  bayes_fit <- suppressWarnings(
    arm::bayesglm(
      formula = formula,
      data = dataset,
      family = binomial(),
      weights = dataset$obs_weights
    )
  )

  prob_train <- predict(bayes_fit, type = "response")
  pred_train <- ifelse(prob_train > 0.5, "Y", "N")
  confmat_train <- caret::confusionMatrix(factor(pred_train, levels = c("N", "Y")), y, positive = "Y")
  auc_train <- pROC::auc(y, prob_train)

  folds <- caret::createFolds(y, k = 10, returnTrain = FALSE)
  cv_truth <- c()
  cv_prob <- c()

  for (i in seq_along(folds)) {
    test_idx <- folds[[i]]
    train_data <- dataset[-test_idx, ]
    test_data <- dataset[test_idx, ]

    bayes_cv_fit <- suppressWarnings(
      arm::bayesglm(
        formula = formula,
        data = train_data,
        family = binomial(),
        weights = train_data$obs_weights
      )
    )

    prob <- predict(bayes_cv_fit, newdata = test_data, type = "response")
    cv_prob <- c(cv_prob, prob)
    cv_truth <- c(cv_truth, as.character(test_data[[response]]))
  }

  truth_cv <- factor(cv_truth, levels = c("N", "Y"))
  prob_cv <- as.numeric(cv_prob)
  pred_cv <- ifelse(prob_cv > 0.5, "Y", "N")

  confmat_cv <- caret::confusionMatrix(factor(pred_cv, levels = c("N", "Y")), truth_cv, positive = "Y")
  auc_cv <- pROC::auc(truth_cv, prob_cv)

  return(list(
    summary_train = tibble::tibble(
      Model = glue::glue("{model_label} (Train Prediction){if (!is.null(sampling_method)) ', Downsampled' else ''}{if (use_weights) ', Weighted' else ''}"),
      AUC = as.numeric(auc_train),
      Sensitivity = confmat_train$byClass["Sensitivity"],
      Specificity = confmat_train$byClass["Specificity"]
    ),
    summary_cv = tibble::tibble(
      Model = glue::glue("{model_label} (CV Prediction){if (!is.null(sampling_method)) ', Downsampled' else ''}{if (use_weights) ', Weighted' else ''}"),
      AUC = as.numeric(auc_cv),
      Sensitivity = confmat_cv$byClass["Sensitivity"],
      Specificity = confmat_cv$byClass["Specificity"]
    ),
    preds_cv = tibble::tibble(
      truth = truth_cv,
      prob = prob_cv
    ),
    bayes_fit = bayes_fit
  ))
}