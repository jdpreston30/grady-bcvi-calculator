#* RF Function
run_rf_analysis <- function(data, model_label, my_seeds, sampling_method = "down") {
  ctrl <- caret::trainControl(
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

  # Optimistic (Train)
  rf_probs_train <- predict(rf_fit, data, type = "prob")[, "Y"]
  rf_preds_train <- predict(rf_fit, data)
  truth_train <- factor(data$stroke, levels = c("N", "Y"))
  rf_confmat_train <- caret::confusionMatrix(rf_preds_train, truth_train, positive = "Y")
  rf_auc_train <- pROC::auc(response = truth_train, predictor = rf_probs_train, levels = c("N", "Y"), direction = "<")

  # Fold-wise metrics
  rf_preds_cv_all <- rf_fit$pred %>%
    filter(mtry == rf_fit$bestTune$mtry) %>%
    group_by(Resample) %>%
    summarise(
      AUC = as.numeric(pROC::auc(factor(obs, levels = c("N", "Y")), Y, levels = c("N", "Y"), direction = "<")),
      Sensitivity = caret::confusionMatrix(
        factor(ifelse(Y > 0.5, "Y", "N"), levels = c("N", "Y")),
        factor(obs, levels = c("N", "Y")),
        positive = "Y"
      )$byClass["Sensitivity"],
      Specificity = caret::confusionMatrix(
        factor(ifelse(Y > 0.5, "Y", "N"), levels = c("N", "Y")),
        factor(obs, levels = c("N", "Y")),
        positive = "Y"
      )$byClass["Specificity"],
      .groups = "drop"
    )

  summary_cv <- rf_preds_cv_all %>%
    summarise(
      Model = paste(model_label, "(CV Prediction, Realistic)"),
      AUC = mean(AUC, na.rm = TRUE),
      Sensitivity = mean(Sensitivity, na.rm = TRUE),
      Specificity = mean(Specificity, na.rm = TRUE)
    )

  summary_train <- tibble(
    Model = paste(model_label, "(Train Prediction, Optimistic)"),
    AUC = as.numeric(rf_auc_train),
    Sensitivity = rf_confmat_train$byClass["Sensitivity"],
    Specificity = rf_confmat_train$byClass["Specificity"]
  )

  varimp <- caret::varImp(rf_fit, scale = TRUE)$importance %>%
    tibble::rownames_to_column("Variable") %>%
    arrange(desc(Overall))

  cv_preds <- rf_fit$pred %>%
    filter(mtry == rf_fit$bestTune$mtry) %>%
    select(obs, Y) %>%
    rename(truth = obs, prob = Y)

  return(list(
    rf_fit = rf_fit,
    summary_train = summary_train,
    summary_cv = summary_cv,
    metrics_cv = rf_preds_cv_all,  # Add fold-level metrics for CI calculation
    preds_cv = cv_preds,
    variable_importance = varimp
  ))
}

#* RF Wrapper
run_rf_all_variants <- function(full_data, simple_data, seeds) {
  results <- list(
    rf_down_full = run_rf_analysis(data = full_data, model_label = "RF Full (Down)", my_seeds = seeds),
    rf_nodown_full = run_rf_analysis(data = full_data, model_label = "RF Full (No Down)", my_seeds = seeds, sampling_method = NULL),
    rf_down_simple = run_rf_analysis(data = simple_data, model_label = "RF Simplified (Down)", my_seeds = seeds),
    rf_nodown_simple = run_rf_analysis(data = simple_data, model_label = "RF Simplified (No Down)", my_seeds = seeds, sampling_method = NULL)
  )
  attr(results$rf_down_full, "Dataset") <- "Complex"
  attr(results$rf_nodown_full, "Dataset") <- "Complex"
  attr(results$rf_down_simple, "Dataset") <- "Simple"
  attr(results$rf_nodown_simple, "Dataset") <- "Simple"
  return(results)
}
