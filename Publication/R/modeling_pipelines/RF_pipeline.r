run_rf_analysis <- function(data, model_label, my_seeds, sampling_method = "down") {
  ctrl <- trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 10,
    seeds = my_seeds,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    sampling = sampling_method,
    savePredictions = "final"
  )

  rf_fit <- caret::train(
    stroke ~ .,
    data = data,
    method = "rf",
    metric = "ROC",
    trControl = ctrl,
    tuneLength = 5
  )

  # Train metrics
  rf_probs_train <- predict(rf_fit, data, type = "prob")[, "Y"]
  rf_preds_train <- predict(rf_fit, data)
  truth_train <- factor(data$stroke, levels = c("N", "Y"))
  rf_confmat_train <- caret::confusionMatrix(rf_preds_train, truth_train, positive = "Y")
  rf_auc_train <- pROC::auc(response = truth_train, predictor = rf_probs_train, levels = c("N", "Y"), direction = "<")

  # CV metrics
  rf_preds_cv <- rf_fit$pred %>%
    filter(mtry == rf_fit$bestTune$mtry)
  truth_cv <- factor(rf_preds_cv$obs, levels = c("N", "Y"))
  preds_cv <- factor(rf_preds_cv$pred, levels = c("N", "Y"))
  probs_cv <- rf_preds_cv$Y
  rf_confmat_cv <- caret::confusionMatrix(preds_cv, truth_cv, positive = "Y")
  rf_auc_cv <- pROC::auc(response = truth_cv, predictor = probs_cv, levels = c("N", "Y"), direction = "<")

  summary_train <- tibble(
    Model = paste(model_label, "(Train Prediction, Optimistic)"),
    AUC = as.numeric(rf_auc_train),
    Sensitivity = rf_confmat_train$byClass["Sensitivity"],
    Specificity = rf_confmat_train$byClass["Specificity"]
  )

  summary_cv <- tibble(
    Model = paste(model_label, "(CV Prediction, Realistic)"),
    AUC = as.numeric(rf_auc_cv),
    Sensitivity = rf_confmat_cv$byClass["Sensitivity"],
    Specificity = rf_confmat_cv$byClass["Specificity"]
  )

  varimp <- caret::varImp(rf_fit, scale = TRUE)$importance %>%
    rownames_to_column("Variable") %>%
    arrange(desc(Overall))

  return(list(
    rf_fit = rf_fit,
    summary_train = summary_train,
    summary_cv = summary_cv,
    variable_importance = varimp
  ))
}

