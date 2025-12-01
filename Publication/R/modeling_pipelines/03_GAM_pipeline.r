#* GAM Function
run_gam_analysis <- function(data, model_label = "GAM", sampling_method = "down", seed = 2025) {
  set.seed(seed)
  data$stroke <- factor(data$stroke, levels = c("N", "Y"))
  y <- data$stroke

  X <- data %>%
    select(-stroke) %>%
    mutate(
      age_ASA = age * as.numeric(as.factor(ASA)),
      age_max_vert = age * max_vert,
      age_max_carotid = age * max_carotid
    ) %>%
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

  gam_fit <- suppressMessages(suppressWarnings(caret::train(
    x = X, y = y,
    method = "gam",
    metric = "ROC",
    trControl = ctrl,
    tuneLength = 5
  )))

  # Optimistic (train set)
  prob_train <- predict(gam_fit, X, type = "prob")[, "Y"]
  class_train <- predict(gam_fit, X)
  suppressMessages(suppressWarnings({
    confmat_train <- caret::confusionMatrix(class_train, y, positive = "Y")
    auc_train <- pROC::auc(y, prob_train)
  }))

  # Fold-level CV summary
  cv_preds <- gam_fit$pred
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
      Model = paste(
        model_label, "(CV Prediction, Realistic)",
        if (!is.null(sampling_method)) "+ downsampling" else ""
      ),
      AUC = mean(AUC, na.rm = TRUE),
      Sensitivity = mean(Sensitivity, na.rm = TRUE),
      Specificity = mean(Specificity, na.rm = TRUE)
    )

  preds_cv_tbl <- gam_fit$pred %>%
    select(truth = obs, prob = Y)

  return(list(
    gam_fit = gam_fit,
    summary_train = tibble::tibble(
      Model = paste(
        model_label, "(Train Prediction, Optimistic)",
        if (!is.null(sampling_method)) "+ downsampling" else ""
      ),
      AUC = as.numeric(auc_train),
      Sensitivity = confmat_train$byClass["Sensitivity"],
      Specificity = confmat_train$byClass["Specificity"]
    ),
    summary_cv = summary_cv,
    preds_cv = preds_cv_tbl
  ))
}

#* GAM Wrapper
run_gam_all_variants <- function(simple_data, seed = 2025) {
  results <- list(
    simpl_down = run_gam_analysis(simple_data, "GAM Simplified", sampling_method = "down", seed = seed),
    simpl_nodown = run_gam_analysis(simple_data, "GAM Simplified", sampling_method = NULL, seed = seed)
  )
  attr(results, "Dataset") <- "Simple"
  return(results)
}
