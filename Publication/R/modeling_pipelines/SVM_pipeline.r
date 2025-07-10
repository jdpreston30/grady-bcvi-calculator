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

  # Train on whole data (optimistic)
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

  # --- FIX 1: save predictions from CV
  cv_result <- suppressMessages(caret::train(
    x = X, y = y,
    method = "svmRadialWeights",
    weights = cv_weights,
    trControl = caret::trainControl(
      method = "repeatedcv", number = 10, repeats = 10,
      classProbs = TRUE, summaryFunction = twoClassSummary,
      savePredictions = "final"
    ),
    metric = "ROC"
  ))

  # --- FIX 2: get held-out predictions from resamples
  cv_preds <- cv_result$pred %>%
    dplyr::filter(sigma == cv_result$bestTune$sigma, C == cv_result$bestTune$C)

  truth_cv <- factor(cv_preds$obs, levels = c("N", "Y"))
  prob_cv <- cv_preds$Y
  pred_class_cv <- ifelse(prob_cv > 0.5, "Y", "N")

  confmat_cv <- caret::confusionMatrix(factor(pred_class_cv, levels = c("N", "Y")), truth_cv, positive = "Y")
  auc_cv <- pROC::auc(truth_cv, prob_cv, levels = c("N", "Y"), direction = "<")

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
    summary_cv = tibble::tibble(
      Model = paste0(
        "SVM (CV Prediction, Realistic)",
        if (use_weights) " + weights" else "",
        if (use_downsampling) " + downsampling" else ""
      ),
      AUC = as.numeric(auc_cv),
      Sensitivity = confmat_cv$byClass["Sensitivity"],
      Specificity = confmat_cv$byClass["Specificity"]
    ),
    preds_cv = tibble::tibble(
      truth = truth_cv,
      prob = prob_cv
    ),
    svm_fit = model
  ))
}
