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
    savePredictions = "final"
  )

  gam_fit <- suppressMessages(suppressWarnings(caret::train(
    x = X, y = y,
    method = "gam",
    metric = "ROC",
    trControl = ctrl,
    tuneLength = 5
  )))

  prob_train <- predict(gam_fit, X, type = "prob")[, "Y"]
  class_train <- predict(gam_fit, X)
  confmat_train <- suppressMessages(suppressWarnings(
    caret::confusionMatrix(class_train, y, positive = "Y")
  ))
  auc_train <- suppressMessages(suppressWarnings(pROC::auc(y, prob_train)))

  cv_preds <- gam_fit$pred
  truth_cv <- factor(cv_preds$obs, levels = c("N", "Y"))
  preds_cv <- factor(cv_preds$pred, levels = c("N", "Y"))
  probs_cv <- cv_preds$Y
  confmat_cv <- suppressMessages(suppressWarnings(
    caret::confusionMatrix(preds_cv, truth_cv, positive = "Y")
  ))
  auc_cv <- suppressMessages(suppressWarnings(pROC::auc(truth_cv, probs_cv)))

  summary_train <- tibble(
    Model = paste(
      model_label, "(Train Prediction, Optimistic)",
      if (!is.null(sampling_method)) "+ downsampling" else ""
    ),
    AUC = as.numeric(auc_train),
    Sensitivity = confmat_train$byClass["Sensitivity"],
    Specificity = confmat_train$byClass["Specificity"]
  )
  summary_cv <- tibble(
    Model = paste(
      model_label, "(CV Prediction, Realistic)",
      if (!is.null(sampling_method)) "+ downsampling" else ""
    ),
    AUC = as.numeric(auc_cv),
    Sensitivity = confmat_cv$byClass["Sensitivity"],
    Specificity = confmat_cv$byClass["Specificity"]
  )

  return(list(
    gam_fit = gam_fit,
    summary_train = summary_train,
    summary_cv = summary_cv
  ))
}
