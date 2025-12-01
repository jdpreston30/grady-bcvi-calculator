get_perf <- function(model, name) {
  res <- model$results
  cat("\nModel:", name, "\n")
  cat("  ROC:  ", round(res$ROC, 4), "\n")
  cat("  Sens: ", round(res$Sens, 4), "\n")
  cat("  Spec: ", round(res$Spec, 4), "\n")
}