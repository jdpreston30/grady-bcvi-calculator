#* Master Modeling Pipeline
run_all_modeling <- function(
    ml_modeling_data,
    my_seeds_rf,
    seed = 2025,
    which_models = "all", # <-- NEW arg
    save_dir = here::here("Publication", "Outputs", "modeling_pipeline_checkpoints"),
    cleanup_checkpoints = TRUE) {
  # --- Load modeling functions ---
  purrr::walk(
    c(
      list.files(here::here("Publication", "R", "modeling_pipelines"), full.names = TRUE),
      list.files(here::here("Publication", "R", "utilities"), full.names = TRUE)
    ),
    ~ suppressMessages(source(.x))
  )

  fs::dir_create(save_dir)
  options(cli.default_handler = function(...) {})

  # --- Prepare datasets ---
  rf_xgb <- ml_modeling_data %>%
    select(-c(tot_vert_inj, max_carotid, tot_carotid_inj, max_vert), -ID)

  rf_xgb_simpl <- ml_modeling_data %>%
    select(stroke, ASA, sexM, age, max_carotid, max_vert)
  # --- Define all modeling steps ---
  all_steps <- list(
    LASSO = quote(run_lasso_all_variants(data = rf_xgb_simpl)),
    `Random Forest` = quote(run_rf_all_variants(full_data = rf_xgb, simple_data = rf_xgb_simpl, seeds = my_seeds_rf)),
    XGBoost = quote(run_xgb_all_variants(full_data = rf_xgb, simple_data = rf_xgb_simpl, repeats = 10, folds = 10, seed_base = seed, save_model = TRUE)),
    SVM = quote(run_svm_all_variants(full_data = rf_xgb, simple_data = rf_xgb_simpl, seed = seed)),
    GAM = quote(run_gam_all_variants(simple_data = rf_xgb_simpl, seed = seed)),
    MLP = quote(run_mlp_all_variants(full_data = rf_xgb, simple_data = rf_xgb_simpl, seed = seed)),
    Bayesian = quote(run_bayeslog_all_variants(
      full_dataset = rf_xgb,
      simple_dataset = rf_xgb_simpl,
      formula_full = as.formula(stroke ~ ASA + sexM + age + Max_LC + Max_RC + Max_LV + Max_RV +
        ASA:age + age:Max_LC + age:Max_RC + age:Max_LV + age:Max_RV),
      formula_simple = as.formula(stroke ~ ASA + sexM + age + max_vert + max_carotid +
        ASA:age + max_vert:age + max_carotid:age),
      seed = seed
    ))
  )
  # --- Filter steps if subset requested ---
  if (!identical(which_models, "all")) {
    all_names <- names(all_steps)
    matched <- all_names[all_names %in% which_models]

    if (length(matched) == 0) {
      stop(glue::glue("❌ Invalid which_models. Valid options: {paste(all_names, collapse=', ')}"))
    }

    steps <- all_steps[matched]
  } else {
    steps <- all_steps
  }

  # --- Progress bar ---
  cat("\n\033[1;36m🚀 Starting modeling pipeline...\033[0m\n\n")
  global_pb <- progress::progress_bar$new(
    format = "\033[1;34mGlobal [:bar] :percent | ETA: :eta | Elapsed: :elapsed\033[0m",
    total = length(steps), clear = FALSE, width = 80
  )

  # --- Run steps ---
  results <- list()

  for (i in seq_along(steps)) {
    name <- names(steps)[i]
    step_name_clean <- tolower(gsub(" ", "_", name))
    path <- file.path(save_dir, glue::glue("{step_name_clean}_all_variants.rds"))
    step_code <- steps[[i]]

    cat("\n", strrep("=", 80), "\n", sep = "")
    cat(glue::glue("\033[1;33m📌 Step {i}/{length(steps)}: {name}\033[0m\n\n"))
    flush.console()

    step_start <- Sys.time()

    result <- if (file.exists(path)) {
      cat("\033[90m✔ Found existing checkpoint. Loading...\033[0m\n\n")
      suppressMessages(readRDS(path))
    } else {
      eval(step_code)
    }

    if (!file.exists(path)) saveRDS(result, path)

    step_end <- Sys.time()
    cat("\n", glue::glue("\033[1;32m✔ Completed {name} in {round(difftime(step_end, step_start, units = 'mins'), 2)} min\033[0m\n"))
    cat(strrep("=", 80), "\n\n", sep = "")

    global_pb$tick(tokens = list(step = name))
    results[[step_name_clean]] <- result
  }

  # --- Save results ---
  if (identical(which_models, "all")) {
    saveRDS(results, here::here("Publication", "Outputs", "all_models.rds"))
  } else {
    saveRDS(results, here::here("Publication", "Outputs", "selected_models.rds"))
  }

  if (cleanup_checkpoints) {
    fs::dir_delete(save_dir)
    cat(glue::glue("\033[90m🧹 Deleted checkpoint directory: {save_dir}\033[0m\n"))
  }

  return(results)
}
