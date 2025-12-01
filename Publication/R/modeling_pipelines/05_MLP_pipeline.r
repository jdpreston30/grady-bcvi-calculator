#* MLP Pipeline
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
    savePredictions = "final",
    returnResamp = "final"
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

  # Optimistic (train set)
  prob_train <- predict(mlp_fit, X, type = "prob")[, "Y"]
  class_train <- predict(mlp_fit, X)
  suppressMessages(suppressWarnings({
    confmat_train <- caret::confusionMatrix(class_train, y, positive = "Y")
    auc_train <- pROC::auc(y, prob_train)
  }))

  # CV evaluation using per-fold stats
  best_size <- mlp_fit$bestTune$size
  cv_preds <- mlp_fit$pred %>%
    filter(size == best_size)

  # Recreate the group identifier (Resample column stores fold identity as e.g. "Fold1.Rep1")
  fold_stats <- cv_preds %>%
    mutate(
      truth = factor(obs, levels = c("N", "Y")),
      pred = factor(pred, levels = c("N", "Y"))
    ) %>%
    group_by(Resample) %>%
    summarise(
      AUC = as.numeric(pROC::auc(truth, Y)),
      Sensitivity = caret::sensitivity(pred, truth, positive = "Y"),
      Specificity = caret::specificity(pred, truth, positive = "Y"),
      .groups = "drop"
    )

  summary_cv <- fold_stats %>%
    summarise(
      Model = paste0(
        model_label, " (CV Prediction, Realistic)",
        if (is.null(sampling_method)) " + no downsampling" else ""
      ),
      AUC = mean(AUC, na.rm = TRUE),
      Sensitivity = mean(Sensitivity, na.rm = TRUE),
      Specificity = mean(Specificity, na.rm = TRUE)
    )

  preds_cv_tbl <- cv_preds %>%
    select(truth = obs, prob = Y)

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
    summary_cv = summary_cv,
    preds_cv = preds_cv_tbl,
    mlp_fit = mlp_fit
  )
}

#* MLP Wrapper
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
