run_caret_model <- function(model_object, dataset, use_weights = TRUE) {
  cv_ctrl <- trainControl(
    method = "cv",
    number = 10,
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  )
  args <- list(
    form = formula(model_object),
    data = dataset,
    method = "glm",
    family = binomial(),
    trControl = cv_ctrl,
    metric = "ROC"
  )
  if (use_weights) {
    args$weights <- dataset$stroke_weight
  }
  trained <- do.call(train, args)
  return(trained)
}