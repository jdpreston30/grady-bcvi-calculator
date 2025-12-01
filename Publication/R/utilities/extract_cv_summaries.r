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