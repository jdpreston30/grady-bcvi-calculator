run_lasso_analysis <- function(data, model_label = "LASSO", use_weights = TRUE) {
  y <- data$stroke
  X <- data %>%
    select(-stroke) %>%
    mutate(
      age_ASA = age * as.numeric(as.factor(ASA)),
      age_max_vert = age * max_vert,
      age_max_carotid = age * max_carotid
    ) %>%
    mutate(across(where(is.factor), ~ as.numeric(as.factor(.)))) %>%
    as.matrix()
  y_bin <- as.numeric(y == "Y")

  weights <- if (use_weights) {
    ifelse(y == "Y", sum(y == "N") / sum(y == "Y"), 1)
  } else {
    rep(1, length(y))
  }

  # CV Repeats
  lasso_repeat_results <- purrr::map_dfr(1:10, function(i) {
    set.seed(2025 + i)
    foldid <- caret::createFolds(y, k = 10, list = FALSE)
    lasso_fit <- suppressMessages(suppressWarnings(glmnet::cv.glmnet(
      x = X, y = y_bin,
      family = "binomial",
      alpha = 1,
      type.measure = "auc",
      foldid = foldid,
      weights = weights
    )))
    lasso_prob <- predict(lasso_fit, newx = X, s = "lambda.min", type = "response")
    lasso_pred <- ifelse(lasso_prob > 0.5, "Y", "N")
    truth <- factor(y, levels = c("N", "Y"))
    suppressMessages(suppressWarnings({
      confmat <- caret::confusionMatrix(factor(lasso_pred, levels = c("N", "Y")), truth, positive = "Y")
      auc_val <- pROC::auc(truth, as.vector(lasso_prob))
    }))
    tibble::tibble(
      rep = i,
      lambda_min = lasso_fit$lambda.min,
      AUC = as.numeric(auc_val),
      Sensitivity = confmat$byClass["Sensitivity"],
      Specificity = confmat$byClass["Specificity"],
      truth = as.character(truth),
      prob = as.vector(lasso_prob)
    )
  })

  # Final model fit
  final_foldid <- caret::createFolds(y, k = 10, list = FALSE)
  lasso_fit_final <- suppressMessages(suppressWarnings(glmnet::cv.glmnet(
    x = X, y = y_bin,
    family = "binomial",
    alpha = 1,
    type.measure = "auc",
    foldid = final_foldid,
    weights = weights
  )))
  lasso_probs_train <- predict(lasso_fit_final, newx = X, s = "lambda.min", type = "response")
  lasso_preds_train <- ifelse(lasso_probs_train > 0.5, "Y", "N")
  truth_train <- factor(y, levels = c("N", "Y"))
  suppressMessages(suppressWarnings({
    confmat_train <- caret::confusionMatrix(factor(lasso_preds_train, levels = c("N", "Y")), truth_train, positive = "Y")
    auc_train <- pROC::auc(truth_train, as.vector(lasso_probs_train))
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
    summary_cv = lasso_repeat_results %>%
      summarise(
        Model = paste(
          model_label, "(CV Prediction, Realistic)",
          if (use_weights) "+ weights" else ""
        ),
        AUC = mean(AUC),
        Sensitivity = mean(Sensitivity),
        Specificity = mean(Specificity)
      ),
    selected_variables = selected_vars,
    preds_cv = lasso_repeat_results %>% select(truth, prob),
    lasso_fit_final = lasso_fit_final
  ))
}
