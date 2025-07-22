run_mlp_model <- function(df, model_label = "MLP", response = "stroke", seed = 2025, sampling_method = "down") {
  set.seed(seed)
  df[[response]] <- factor(df[[response]], levels = c("N", "Y"))
  y <- df[[response]]

  X <- df %>%
    select(-all_of(response)) %>%
    mutate(across(where(is.factor), ~ as.numeric(as.factor(.)))) %>%
    as.data.frame()

  ctrl <- caret::trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 10,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    sampling = sampling_method,
    savePredictions = "final"
  )

  suppressMessages(suppressWarnings({
    mlp_fit <- caret::train(
      x = X,
      y = y,
      method = "nnet",
      metric = "ROC",
      trControl = ctrl,
      tuneLength = 5,
      trace = FALSE
    )
  }))

  # Optimistic
  prob_train <- predict(mlp_fit, X, type = "prob")[, "Y"]
  class_train <- predict(mlp_fit, X)
  suppressMessages(suppressWarnings({
    confmat_train <- caret::confusionMatrix(class_train, y, positive = "Y")
    auc_train <- pROC::auc(y, prob_train)
  }))

  # Realistic (from CV predictions)
  cv_preds <- mlp_fit$pred %>%
    filter(size == mlp_fit$bestTune$size)
  truth_cv <- factor(cv_preds$obs, levels = c("N", "Y"))
  preds_cv <- factor(cv_preds$pred, levels = c("N", "Y"))
  probs_cv <- cv_preds$Y
  suppressMessages(suppressWarnings({
    confmat_cv <- caret::confusionMatrix(preds_cv, truth_cv, positive = "Y")
    auc_cv <- pROC::auc(truth_cv, probs_cv)
  }))

  list(
    summary_train = tibble::tibble(
      Model = paste0(
        model_label, " (Train Prediction, Optimistic)",
        if (is.null(sampling_method)) " + no downsampling" else ""
      ),
      AUC = as.numeric(auc_train),
      Sensitivity = confmat_train$byClass["Sensitivity"],
      Specificity = confmat_train$byClass["Specificity"]
    ),
    summary_cv = tibble::tibble(
      Model = paste0(
        model_label, " (CV Prediction, Realistic)",
        if (is.null(sampling_method)) " + no downsampling" else ""
      ),
      AUC = as.numeric(auc_cv),
      Sensitivity = confmat_cv$byClass["Sensitivity"],
      Specificity = confmat_cv$byClass["Specificity"]
    ),
    mlp_fit = mlp_fit
  )
  }

  run_mlp_all_variants <- function(full_data, simple_data, seed = 2025) {
  cat("\033[1;33;1mStarting MLP analysis...\033[0m\n")
  pb <- progress::progress_bar$new(
    format = "[:bar] :current/:total (:percent) ETA: :eta",
    total = 4, clear = FALSE, width = 60
  )
  results <- list()
  pb$tick()
  results$full_down <- run_mlp_model(full_data, model_label = "MLP Full", sampling_method = "down", seed = seed)
  pb$tick()
  results$full_nodown <- run_mlp_model(full_data, model_label = "MLP Full", sampling_method = NULL, seed = seed)
  pb$tick()
  results$simpl_down <- run_mlp_model(simple_data, model_label = "MLP Simplified", sampling_method = "down", seed = seed)
  pb$tick()
  results$simpl_nodown <- run_mlp_model(simple_data, model_label = "MLP Simplified", sampling_method = NULL, seed = seed)
  cat("\033[1;32mMLP analysis complete!\033[0m\n")
  return(results)
}
