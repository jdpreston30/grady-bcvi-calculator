# All wrappers rewritten with standardized `Dataset` attribute tagging

run_lasso_all_variants <- function(data) {
  results <- list(
    weighted = run_lasso_analysis(data, model_label = "LASSO", use_weights = TRUE),
    unweighted = run_lasso_analysis(data, model_label = "LASSO", use_weights = FALSE)
  )
  attr(results, "Dataset") <- "Simple"
  return(results)
}

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

run_svm_all_variants <- function(full_data, simple_data, seed = 2025) {
  results <- list(
    full_weighted = run_svm_model(full_data, seed = seed, use_weights = TRUE, use_downsampling = FALSE),
    full_downsampled = run_svm_model(full_data, seed = seed, use_weights = FALSE, use_downsampling = TRUE),
    full_both = run_svm_model(full_data, seed = seed, use_weights = TRUE, use_downsampling = TRUE),
    full_unadjusted = run_svm_model(full_data, seed = seed, use_weights = FALSE, use_downsampling = FALSE),
    simpl_weighted = run_svm_model(simple_data, seed = seed, use_weights = TRUE, use_downsampling = FALSE),
    simpl_downsampled = run_svm_model(simple_data, seed = seed, use_weights = FALSE, use_downsampling = TRUE),
    simpl_both = run_svm_model(simple_data, seed = seed, use_weights = TRUE, use_downsampling = TRUE),
    simpl_unadjusted = run_svm_model(simple_data, seed = seed, use_weights = FALSE, use_downsampling = FALSE)
  )
  attr(results$full_weighted, "Dataset") <- "Complex"
  attr(results$full_downsampled, "Dataset") <- "Complex"
  attr(results$full_both, "Dataset") <- "Complex"
  attr(results$full_unadjusted, "Dataset") <- "Complex"
  attr(results$simpl_weighted, "Dataset") <- "Simple"
  attr(results$simpl_downsampled, "Dataset") <- "Simple"
  attr(results$simpl_both, "Dataset") <- "Simple"
  attr(results$simpl_unadjusted, "Dataset") <- "Simple"
  return(results)
}

run_xgb_all_variants <- function(full_data, simple_data, repeats = 10, folds = 10, seed_base = 2025, save_model = FALSE) {
  results <- list(
    full_weight_down = run_xgb_analysis(full_data, "XGB Weighted", repeats, folds, seed_base, TRUE, TRUE, save_model),
    full_unweight_nodown = run_xgb_analysis(full_data, "XGB Unweighted", repeats, folds, seed_base, FALSE, FALSE, save_model),
    full_down_only = run_xgb_analysis(full_data, "XGB Downsample Only", repeats, folds, seed_base, FALSE, TRUE, save_model),
    full_weight_only = run_xgb_analysis(full_data, "XGB Weight Only", repeats, folds, seed_base, TRUE, FALSE, save_model),
    simpl_weight_down = run_xgb_analysis(simple_data, "XGB Weighted, simplified", repeats, folds, seed_base, TRUE, TRUE, save_model),
    simpl_unweight_nodown = run_xgb_analysis(simple_data, "XGB Unweighted, simplified", repeats, folds, seed_base, FALSE, FALSE, save_model),
    simpl_down_only = run_xgb_analysis(simple_data, "XGB Downsample Only, simplified", repeats, folds, seed_base, FALSE, TRUE, save_model),
    simpl_weight_only = run_xgb_analysis(simple_data, "XGB Weight Only, simplified", repeats, folds, seed_base, TRUE, FALSE, save_model)
  )
  purrr::walk(names(results), function(n) {
    attr(results[[n]], "Dataset") <- ifelse(grepl("simpl", n), "Simple", "Complex")
  })
  return(results)
}

run_gam_all_variants <- function(simple_data, seed = 2025) {
  results <- list(
    simpl_down = run_gam_analysis(simple_data, "GAM Simplified", sampling_method = "down", seed = seed),
    simpl_nodown = run_gam_analysis(simple_data, "GAM Simplified", sampling_method = NULL, seed = seed)
  )
  attr(results, "Dataset") <- "Simple"
  return(results)
}

run_bayeslog_all_variants <- function(full_dataset, simple_dataset, formula_full, formula_simple, seed = 2025) {
  results <- list(
    full_down = run_bayeslog_model(
      dataset = full_dataset,
      formula = formula_full,
      model_label = "Bayesian Full",
      seed = seed,
      sampling_method = "down",
      use_weights = FALSE
    ),
    full_nodown = run_bayeslog_model(
      dataset = full_dataset,
      formula = formula_full,
      model_label = "Bayesian Full",
      seed = seed,
      sampling_method = NULL,
      use_weights = FALSE
    ),
    simpl_down = run_bayeslog_model(
      dataset = simple_dataset,
      formula = formula_simple,
      model_label = "Bayesian Simplified",
      seed = seed,
      sampling_method = "down",
      use_weights = FALSE
    ),
    simpl_nodown = run_bayeslog_model(
      dataset = simple_dataset,
      formula = formula_simple,
      model_label = "Bayesian Simplified",
      seed = seed,
      sampling_method = NULL,
      use_weights = FALSE
    )
  )
  attr(results$full_down, "Dataset") <- "Complex"
  attr(results$full_nodown, "Dataset") <- "Complex"
  attr(results$simpl_down, "Dataset") <- "Simple"
  attr(results$simpl_nodown, "Dataset") <- "Simple"
  return(results)
}
