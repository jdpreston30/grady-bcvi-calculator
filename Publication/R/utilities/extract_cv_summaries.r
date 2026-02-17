extract_cv_summaries <- function(model_name, model_object) {
  purrr::map_dfr(names(model_object), function(variant) {
    slot <- model_object[[variant]]
    if (!is.null(slot$summary_cv)) {
      tibble::tibble(
        Model = paste0(model_name, "_", variant),
        AUC = slot$summary_cv$AUC,
        Sensitivity = slot$summary_cv$Sensitivity,
        Specificity = slot$summary_cv$Specificity
      )
    } else {
      tibble::tibble(
        Model = paste0(model_name, "_", variant),
        AUC = NA_real_,
        Sensitivity = NA_real_,
        Specificity = NA_real_
      )
    }
  })
}

#* Extract CV summaries with 95% confidence intervals
extract_cv_summaries_with_ci <- function(model_name, model_object) {
  purrr::map_dfr(names(model_object), function(variant) {
    slot <- model_object[[variant]]
    
    # Check if we have fold-level metrics
    if (!is.null(slot$metrics_cv)) {
      # For models that store metrics_cv (Bayes, custom implementations)
      fold_data <- slot$metrics_cv
    } else if (!is.null(slot$cv_fold_metrics)) {
      # For models that might use a different name
      fold_data <- slot$cv_fold_metrics
    } else {
      # Try to extract from summary_cv and return without CIs
      if (!is.null(slot$summary_cv)) {
        return(tibble::tibble(
          Model = paste0(model_name, "_", variant),
          AUC = slot$summary_cv$AUC,
          AUC_CI_lower = NA_real_,
          AUC_CI_upper = NA_real_,
          Sensitivity = slot$summary_cv$Sensitivity,
          Sens_CI_lower = NA_real_,
          Sens_CI_upper = NA_real_,
          Specificity = slot$summary_cv$Specificity,
          Spec_CI_lower = NA_real_,
          Spec_CI_upper = NA_real_
        ))
      } else {
        return(tibble::tibble(
          Model = paste0(model_name, "_", variant),
          AUC = NA_real_,
          AUC_CI_lower = NA_real_,
          AUC_CI_upper = NA_real_,
          Sensitivity = NA_real_,
          Sens_CI_lower = NA_real_,
          Sens_CI_upper = NA_real_,
          Specificity = NA_real_,
          Spec_CI_lower = NA_real_,
          Spec_CI_upper = NA_real_
        ))
      }
    }
    
    # Calculate mean and 95% CI using percentile method
    tibble::tibble(
      Model = paste0(model_name, "_", variant),
      AUC = mean(fold_data$AUC, na.rm = TRUE),
      AUC_CI_lower = quantile(fold_data$AUC, 0.025, na.rm = TRUE),
      AUC_CI_upper = quantile(fold_data$AUC, 0.975, na.rm = TRUE),
      Sensitivity = mean(fold_data$Sensitivity, na.rm = TRUE),
      Sens_CI_lower = quantile(fold_data$Sensitivity, 0.025, na.rm = TRUE),
      Sens_CI_upper = quantile(fold_data$Sensitivity, 0.975, na.rm = TRUE),
      Specificity = mean(fold_data$Specificity, na.rm = TRUE),
      Spec_CI_lower = quantile(fold_data$Specificity, 0.025, na.rm = TRUE),
      Spec_CI_upper = quantile(fold_data$Specificity, 0.975, na.rm = TRUE)
    )
  })
}