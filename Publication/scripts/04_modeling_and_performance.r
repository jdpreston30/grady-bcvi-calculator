#* 4: Run multiple modeling methods
  #+ 4.0: Data and function setup
    #- 4.0.1: Load the modeling data (save for later, performed in pipeline automatically)
      #_Without tot variable, complex
      rf_xgb <- ml_modeling_data %>%
        select(-c(tot_vert_inj, max_carotid, tot_carotid_inj, max_vert), -ID)
      # _Without tot variable, simple
      rf_xgb_simpl <- ml_modeling_data %>%
        select(stroke, ASA, sexM, age, max_carotid, max_vert)
    #- 4.0.2: Load the modeling functions
      purrr::walk(
        list.files(here::here("R", "modeling_pipelines"), full.names = TRUE),
        source
      )
      source(here::here("Publication", "R", "master_modeling_pipeline.R"))
  #+ 4.1: Run master pipeline
    all_model_results <- run_all_modeling(
      ml_modeling_data = ml_modeling_data,
      my_seeds_rf = my_seeds_rf,
      seed = 2025
    )
  #+ 4.2: Extract each model's results for use below (optional unpacking)
    lasso_all_variants <- all_model_results$lasso
    rf_all_variants <- run_rf_all_variants(full_data = rf_xgb, simple_data = rf_xgb_simpl, seeds = my_seeds_rf) # temp
    xgb_all_variants   <- all_model_results$xgb
    svm_all_variants <- run_svm_model(rf_xgb, response = "stroke", seed = 2025, use_weights = FALSE, use_downsampling = TRUE) # temp
    gam_all_variants   <- all_model_results$gam
    mlp_all_variants   <- all_model_results$mlp
    bayes_all_variants <- all_model_results$bayes
  #+ 4.3: Create summary table of model performance
    #- 4.3.1: List all non-Bayesian models
      model_list <- list(
        LASSO = lasso_all_variants,
        RF    = rf_all_variants,
        XGB   = xgb_all_variants,
        SVM   = svm_all_variants,
        GAM   = gam_all_variants,
        MLP   = mlp_all_variants
      )
    #- 4.3.2: Extract summary_cv from all inner slots
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
      non_bayes_summary <- purrr::map_dfr(names(model_list), function(name) {
        extract_cv_summaries(name, model_list[[name]])
      })
    #- 4.3.3: Bayesian models use summary_train instead
      bayes_summary <- purrr::map_dfr(names(bayes_all_variants), function(variant) {
        model <- bayes_all_variants[[variant]]
        if (!is.null(model$summary_train)) {
          tibble::tibble(
            Model = paste0("Bayes_", variant),
            AUC = model$summary_train$AUC,
            Sensitivity = model$summary_train$Sensitivity,
            Specificity = model$summary_train$Specificity
          )
        } else {
          tibble::tibble(
            Model = paste0("Bayes_", variant),
            AUC = NA_real_,
            Sensitivity = NA_real_,
            Specificity = NA_real_
          )
        }
      })
    #- 4.3.4: Manually specify the metadata for each model
      manual_metadata <- tribble(
        ~Model, ~Method, ~Dataset, ~Weighting, ~Downsampling, ~Category, ~Feature_Selection,
        "SVM_full_downsampled", "SVM", "Full", "N", "Y", "Margin-based classifier", "None",
        "SVM_full_both", "SVM", "Full", "Y", "Y", "Margin-based classifier", "None",
        "SVM_simpl_downsampled", "SVM", "Simple", "N", "Y", "Margin-based classifier", "None",
        "SVM_simpl_both", "SVM", "Simple", "Y", "Y", "Margin-based classifier", "None",
        "Bayes_simpl_down", "Bayes", "Simple", "N", "Y", "Bayesian GLM", "None",
        "Bayes_full_down", "Bayes", "Full", "N", "Y", "Bayesian GLM", "None",
        "LASSO_weighted", "LASSO", "Simple", "Y", "N", "Penalized GLM", "Built-in",
        "MLP_simpl_down", "MLP", "Simple", "N", "Y", "Neural network", "None",
        "GAM_simpl_down", "GAM", "Simple", "N", "Y", "Additive spline model", "None",
        "RF_rf_down_full", "RF", "Full", "N", "Y", "Ensemble (bagging)", "Implicit",
        "MLP_full_down", "MLP", "Full", "N", "Y", "Neural network", "None",
        "RF_rf_down_simple", "RF", "Simple", "N", "Y", "Ensemble (bagging)", "Implicit",
        "XGB_simpl_down_only", "XGB", "Simple", "N", "Y", "Ensemble (boosting)", "Implicit",
        "XGB_full_down_only", "XGB", "Full", "N", "Y", "Ensemble (boosting)", "Implicit",
        "XGB_full_weight_only", "XGB", "Full", "Y", "N", "Ensemble (boosting)", "Implicit",
        "XGB_simpl_weight_only", "XGB", "Simple", "Y", "N", "Ensemble (boosting)", "Implicit",
        "XGB_full_weight_down", "XGB", "Full", "Y", "Y", "Ensemble (boosting)", "Implicit",
        "XGB_simpl_weight_down", "XGB", "Simple", "Y", "Y", "Ensemble (boosting)", "Implicit",
        "XGB_simpl_unweight_nodown", "XGB", "Simple", "N", "N", "Ensemble (boosting)", "Implicit",
        "SVM_full_weighted", "SVM", "Full", "Y", "N", "Margin-based classifier", "None",
        "SVM_full_unadjusted", "SVM", "Full", "N", "N", "Margin-based classifier", "None",
        "GAM_simpl_nodown", "GAM", "Simple", "N", "N", "Additive spline model", "None",
        "RF_rf_nodown_simple", "RF", "Simple", "N", "N", "Ensemble (bagging)", "Implicit",
        "SVM_simpl_weighted", "SVM", "Simple", "Y", "N", "Margin-based classifier", "None",
        "SVM_simpl_unadjusted", "SVM", "Simple", "N", "N", "Margin-based classifier", "None",
        "Bayes_simpl_nodown", "Bayes", "Simple", "N", "N", "Bayesian GLM", "None",
        "Bayes_full_nodown", "Bayes", "Full", "N", "N", "Bayesian GLM", "None",
        "XGB_full_unweight_nodown", "XGB", "Full", "N", "N", "Ensemble (boosting)", "Implicit",
        "MLP_full_nodown", "MLP", "Full", "N", "N", "Neural network", "None",
        "RF_rf_nodown_full", "RF", "Full", "N", "N", "Ensemble (bagging)", "Implicit",
        "MLP_simpl_nodown", "MLP", "Simple", "N", "N", "Neural network", "None",
        "LASSO_unweighted", "LASSO", "Simple", "N", "N", "Penalized GLM", "Built-in"
      )
    #- 4.3.5: Combine and annotate with metadata for each model, export
      all_model_summary <- dplyr::bind_rows(non_bayes_summary, bayes_summary) %>%
        mutate(
          Youdens_J = Sensitivity + Specificity - 1
        ) %>%
        left_join(manual_metadata, by = "Model") %>%
        mutate(
          Sensitivity = paste0(round(Sensitivity * 100, 0), "%"),
          Specificity = paste0(round(Specificity * 100, 0), "%")
        ) %>%
        mutate(across(where(is.double) & !c(Sensitivity, Specificity), ~ round(.x, 2))) %>%
        select(Method, Category, Feature_Selection, Youdens_J, AUC, Sensitivity, Specificity, everything()) %>%
        arrange(desc(Dataset), desc(Weighting), desc(Downsampling), desc(Youdens_J))
      write.csv(all_model_summary, "all_model_summary.csv", row.names = FALSE)
    #- 4.3.6: Choose the ideal model for each method
      ideal_model_summary <- all_model_summary %>%
        filter(Model %in% c(
          "SVM_full_downsampled",
          "RF_rf_down_full",
          "Bayes_simpl_down",
          "MLP_simpl_down",
          "GAM_simpl_down",
          "XGB_simpl_down_only",
          "LASSO_weighted"
        )) %>%
        mutate(
          Sampling_Method = case_when(
            Model == "SVM_full_downsampled" ~ "F,D",
            Model == "RF_rf_down_full" ~ "F,D",
            Model == "Bayes_simpl_down" ~ "S,D",
            Model == "MLP_simpl_down" ~ "S,D",
            Model == "GAM_simpl_down" ~ "S,D",
            Model == "XGB_simpl_down_only" ~ "S,D",
            Model == "LASSO_weighted" ~ "S,W"
          ))
      write.csv(ideal_model_summary, "ideal_model_summary.csv", row.names = FALSE)
