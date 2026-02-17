#* SVM Function
#* SVM Function (Updated: Averaging CV metrics manually)
run_svm_model <- function(df, response = "stroke", seed = 2025, use_weights = TRUE, use_downsampling = FALSE) {
  set.seed(seed)
  df[[response]] <- factor(df[[response]], levels = c("N", "Y"))
  y <- df[[response]]

  if (use_downsampling) {
    df <- df %>%
      group_by(!!sym(response)) %>%
      sample_n(min(table(df[[response]]))) %>%
      ungroup()
    y <- df[[response]]
  }

  X <- df %>%
    select(-all_of(response)) %>%
    mutate(across(where(is.factor), ~ as.numeric(as.factor(.)))) %>%
    as.data.frame()

  class_weights <- if (use_weights) {
    list(N = 1, Y = sum(y == "N") / sum(y == "Y"))
  } else {
    NULL
  }

  # Train on full data (optimistic)
  model <- e1071::svm(
    x = X,
    y = y,
    class.weights = class_weights,
    probability = TRUE,
    kernel = "radial"
  )

  pred_prob <- attr(predict(model, X, probability = TRUE), "probabilities")[, "Y"]
  pred_class <- ifelse(pred_prob > 0.5, "Y", "N")

  confmat_train <- caret::confusionMatrix(factor(pred_class, levels = c("N", "Y")), y, positive = "Y")
  auc_train <- pROC::auc(response = y, predictor = pred_prob, levels = c("N", "Y"), direction = "<")

  # CV weights
  cv_weights <- if (use_weights) unname(unlist(class_weights[y])) else rep(1, length(y))

  # Repeated CV setup via caret
  ctrl <- caret::trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 10,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    savePredictions = "final"
  )

  cv_result <- suppressMessages(caret::train(
    x = X, y = y,
    method = "svmRadialWeights",
    weights = cv_weights,
    trControl = ctrl,
    metric = "ROC"
  ))

  # Fold-wise predictions for best parameters
  cv_preds <- cv_result$pred %>%
    filter(sigma == cv_result$bestTune$sigma, C == cv_result$bestTune$C)

  # Fold-level summarization with numeric AUC
  cv_metrics <- cv_preds %>%
    group_by(Resample) %>%
    summarise(
      AUC = as.numeric(pROC::auc(factor(obs, levels = c("N", "Y")), Y)),
      Sensitivity = caret::sensitivity(factor(ifelse(Y > 0.5, "Y", "N"), levels = c("N", "Y")),
        factor(obs, levels = c("N", "Y")),
        positive = "Y"
      ),
      Specificity = caret::specificity(factor(ifelse(Y > 0.5, "Y", "N"), levels = c("N", "Y")),
        factor(obs, levels = c("N", "Y")),
        positive = "Y"
      ),
      .groups = "drop"
    )

  summary_cv <- cv_metrics %>% summarise(
    Model = paste0(
      "SVM (CV Prediction, Realistic)",
      if (use_weights) " + weights" else "",
      if (use_downsampling) " + downsampling" else ""
    ),
    AUC = mean(AUC, na.rm = TRUE),
    Sensitivity = mean(Sensitivity, na.rm = TRUE),
    Specificity = mean(Specificity, na.rm = TRUE)
  )

  return(list(
    summary_train = tibble::tibble(
      Model = paste0(
        "SVM (Train Prediction, Optimistic)",
        if (use_weights) " + weights" else "",
        if (use_downsampling) " + downsampling" else ""
      ),
      AUC = as.numeric(auc_train),
      Sensitivity = confmat_train$byClass["Sensitivity"],
      Specificity = confmat_train$byClass["Specificity"]
    ),
    summary_cv = summary_cv,
    metrics_cv = cv_metrics,  # Add fold-level metrics for CI calculation
    preds_cv = cv_preds %>% select(truth = obs, prob = Y),
    svm_fit = model
  ))
}

#* SVM Wrapper
run_svm_all_variants <- function(full_data, simple_data, seed = 2025) {
  results <- list(
    full_weighted = run_svm_model(full_data, seed = seed, use_weights = TRUE, use_downsampling = FALSE),
    full_downsampled = run_svm_model(full_data, seed = seed, use_weights = FALSE, use_downsampling = TRUE),
    full_both = run_svm_model(full_data, seed = seed, use_weights = TRUE, use_downsampling = TRUE),
    full_unadjusted = run_svm_model(full_data, seed = seed, use_weights = FALSE, use_downsampling = FALSE),
    simpl_weighted = run_svm_model(simple_data, seed = seed, use_weights = TRUE, use_downsampling = FALSE),
    simpl_downsampled = run_svm_model(simple_data, seed = seed, use_weights = FALSE, use_downsampling = TRUE),
    simpl_both = run_svm_model(simple_data, seed = seed, use_weights = TRUE, use_downsampling = TRUE),
    simpl_unadjusted = run_svm_model(simple_data, seed = seed, use_weights = FALSE, use_downsampling = FALSE)
  )
  attr(results$full_weighted, "Dataset") <- "Complex"
  attr(results$full_downsampled, "Dataset") <- "Complex"
  attr(results$full_both, "Dataset") <- "Complex"
  attr(results$full_unadjusted, "Dataset") <- "Complex"
  attr(results$simpl_weighted, "Dataset") <- "Simple"
  attr(results$simpl_downsampled, "Dataset") <- "Simple"
  attr(results$simpl_both, "Dataset") <- "Simple"
  attr(results$simpl_unadjusted, "Dataset") <- "Simple"
  return(results)
}