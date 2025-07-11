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
    rf_all_variants <- all_model_results$random_forest
    xgb_all_variants <- all_model_results$xgboost
    svm_all_variants <- all_model_results$svm
    gam_all_variants <- all_model_results$gam
    mlp_all_variants <- all_model_results$mlp
    bayes_all_variants <- all_model_results$bayes
  #+ 4.3: Create summary table of model performance
    #- 4.3.1: List all non-Bayes models
      model_list <- list(
        LASSO = lasso_all_variants,
        RF    = rf_all_variants,
        XGB   = xgb_all_variants,
        SVM   = svm_all_variants,
        GAM   = gam_all_variants,
        MLP   = mlp_all_variants,
        Bayes = bayes_all_variants
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
      model_summary <- purrr::map_dfr(names(model_list), function(name) {
        extract_cv_summaries(name, model_list[[name]])
      })
    #- 4.3.3: Manually specify the metadata for each model
      manual_metadata <- tribble(
        ~Model, ~Method, ~Dataset, ~Weighting, ~Downsampling, ~Category, ~Feature_Selection,
        "SVM_full_downsampled", "SVM", "Full", "N", "Y", "Margin-based classifier", "None",
        "SVM_full_both", "SVM", "Full", "Y", "Y", "Margin-based classifier", "None",
        "SVM_simpl_downsampled", "SVM", "Simple", "N", "Y", "Margin-based classifier", "None",
        "SVM_simpl_both", "SVM", "Simple", "Y", "Y", "Margin-based classifier", "None",
        "Bayes_simpl_down", "Bayes", "Simple", "N", "Y", "Bayes GLM", "None",
        "Bayes_full_down", "Bayes", "Full", "N", "Y", "Bayes GLM", "None",
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
        "Bayes_simpl_nodown", "Bayes", "Simple", "N", "N", "Bayes GLM", "None",
        "Bayes_full_nodown", "Bayes", "Full", "N", "N", "Bayes GLM", "None",
        "XGB_full_unweight_nodown", "XGB", "Full", "N", "N", "Ensemble (boosting)", "Implicit",
        "MLP_full_nodown", "MLP", "Full", "N", "N", "Neural network", "None",
        "RF_rf_nodown_full", "RF", "Full", "N", "N", "Ensemble (bagging)", "Implicit",
        "MLP_simpl_nodown", "MLP", "Simple", "N", "N", "Neural network", "None",
        "LASSO_unweighted", "LASSO", "Simple", "N", "N", "Penalized GLM", "Built-in"
      )
    #- 4.3.4: Combine and annotate with metadata for each model, export
      all_model_summary <- model_summary %>%
        mutate(
          Youdens_J = Sensitivity + Specificity - 1
        ) %>%
        left_join(manual_metadata, by = "Model") %>%
        mutate(
          Sensitivity = paste0(round(Sensitivity * 100, 0), "%"),
          Specificity = paste0(round(Specificity * 100, 0), "%")
        ) %>%
        mutate(
          Sampling_Method = paste0(
            if_else(Dataset == "Full", "F", "S"),
            case_when(
              Weighting == "Y" & Downsampling == "Y" ~ ",W,D",
              Weighting == "Y" & Downsampling == "N" ~ ",W",
              Weighting == "N" & Downsampling == "Y" ~ ",D",
              TRUE ~ ""
            )
          ),
          # Prioritization ranking for tie-breaking
          tie_breaker = case_when(
            Dataset == "Simple" & Downsampling == "Y" & Weighting == "N" ~ 1,
            Dataset == "Simple" & Downsampling == "Y" & Weighting == "Y" ~ 2,
            Dataset == "Simple" & Downsampling == "N" & Weighting == "Y" ~ 3,
            Dataset == "Simple" & Downsampling == "N" & Weighting == "N" ~ 4,
            Dataset == "Full"   & Downsampling == "Y" & Weighting == "N" ~ 5,
            Dataset == "Full"   & Downsampling == "Y" & Weighting == "Y" ~ 6,
            Dataset == "Full"   & Downsampling == "N" & Weighting == "Y" ~ 7,
            Dataset == "Full"   & Downsampling == "N" & Weighting == "N" ~ 8,
            TRUE ~ 9
          )
        ) %>%
        group_by(Method) %>%
        arrange(desc(Youdens_J), tie_breaker, .by_group = TRUE) %>%
        mutate(Chosen = if_else(row_number() == 1, "Y", "N")) %>%
        ungroup() %>%
        mutate(across(where(is.double) & !c(Sensitivity, Specificity), ~ round(.x, 2))) %>%
        select(Method, Category, Feature_Selection, Youdens_J, AUC, Sensitivity, Specificity,
              Model, Dataset, Weighting, Downsampling, Sampling_Method, Chosen)
      write.xlsx(all_model_summary, "model_summary.xlsx")
