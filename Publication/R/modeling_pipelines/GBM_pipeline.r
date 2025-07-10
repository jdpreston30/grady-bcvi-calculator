run_xgb_analysis <- function(data, model_label = "XGBoost", repeats = 10, folds = 10, seed_base = 2025,
                              use_weights = TRUE, use_downsampling = TRUE, save_model = FALSE) {
  y <- data$stroke
  X <- data %>%
    select(-stroke) %>%
    mutate(across(where(is.factor), ~ as.numeric(as.factor(.)))) %>%
    as.matrix()
  y_bin <- as.numeric(y == "Y")
  pos_weight <- if (use_weights) sum(y_bin == 0) / sum(y_bin == 1) else 1

  cv_results <- purrr::map_dfr(seq_len(repeats), function(i) {
    set.seed(seed_base + i)
    fold_id <- caret::createFolds(y_bin, k = folds, list = FALSE)
    purrr::map_dfr(seq_len(folds), function(k) {
      test_idx <- which(fold_id == k)
      train_idx <- which(fold_id != k)
      train_df <- data[train_idx, ]
      if (use_downsampling) {
        train_df <- train_df %>%
          group_by(stroke) %>%
          sample_n(min(table(train_df$stroke))) %>%
          ungroup()
      }
      dtrain <- xgboost::xgb.DMatrix(
        data = train_df %>%
          select(-stroke) %>%
          mutate(across(where(is.factor), ~ as.numeric(as.factor(.)))) %>%
          as.matrix(),
        label = as.numeric(train_df$stroke == "Y")
      )
      dtest <- xgboost::xgb.DMatrix(data = X[test_idx, ], label = y_bin[test_idx])
      xgb_fit <- xgboost::xgb.train(
        data = dtrain,
        objective = "binary:logistic",
        eval_metric = "auc",
        nrounds = 100,
        verbose = 0,
        params = list(
          scale_pos_weight = pos_weight,
          max_depth = 3,
          eta = 0.1
        )
      )
      prob <- predict(xgb_fit, dtest)
      pred <- factor(ifelse(prob > 0.5, "Y", "N"), levels = c("N", "Y"))
      truth <- factor(y[test_idx], levels = c("N", "Y"))
      suppressMessages(suppressWarnings({
        confmat <- caret::confusionMatrix(pred, truth, positive = "Y")
        auc_val <- as.numeric(pROC::auc(truth, prob))
      }))
      tibble(
        rep = i,
        fold = k,
        AUC = auc_val,
        Sensitivity = confmat$byClass["Sensitivity"],
        Specificity = confmat$byClass["Specificity"]
      )
    })
  })

  summary_cv <- cv_results %>%
    summarise(
      Model = paste(model_label, "(CV Prediction, Realistic)"),
      AUC = mean(AUC),
      Sensitivity = mean(Sensitivity),
      Specificity = mean(Specificity)
    )

  # Fit final model on all data
  dtrain_full <- xgboost::xgb.DMatrix(data = X, label = y_bin)
  xgb_fit_final <- xgboost::xgb.train(
    data = dtrain_full,
    objective = "binary:logistic",
    eval_metric = "auc",
    nrounds = 100,
    verbose = 0,
    params = list(
      scale_pos_weight = pos_weight,
      max_depth = 3,
      eta = 0.1
    )
  )

  prob_train <- predict(xgb_fit_final, dtrain_full)
  pred_train <- factor(ifelse(prob_train > 0.5, "Y", "N"), levels = c("N", "Y"))
  truth_train <- factor(y, levels = c("N", "Y"))
  suppressMessages(suppressWarnings({
    confmat_train <- caret::confusionMatrix(pred_train, truth_train, positive = "Y")
    auc_train <- as.numeric(pROC::auc(truth_train, prob_train))
  }))

  summary_train <- tibble(
    Model = paste(model_label, "(Train Prediction, Optimistic)"),
    AUC = auc_train,
    Sensitivity = confmat_train$byClass["Sensitivity"],
    Specificity = confmat_train$byClass["Specificity"]
  )

  if (save_model) {
    return(list(
      summary_cv = summary_cv,
      summary_train = summary_train,
      final_model = xgb_fit_final
    ))
  } else {
    return(list(
      summary_cv = summary_cv,
      summary_train = summary_train
    ))
  }
}