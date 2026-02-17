# Save root directory
root_dir <- getwd()

# Publication Analysis Pipeline (uses Publication renv)
{
  setwd(file.path(root_dir, "Publication"))
  renv::restore(prompt = FALSE)
  source("scripts/00_dependencies_and_seeds.r")
  
  # Configure data paths - CHANGE 'which_computer' TO SWITCH BETWEEN MACHINES
  # Options: "laptop", "desktop", or "other" (prompts for custom paths)
  load_raw_data(which_computer = "desktop")
  
  source("scripts/01_import_and_preprocess.r")
  source("scripts/02_descriptive_statistics.r")
  source("scripts/03_trad_linear_modeling.r")
  source("scripts/04_modeling_and_performance.r")
  source("scripts/05_ROC_construction.r")
  source("scripts/06_risk_simulations.r")
  source("scripts/07_asa_timing.r")
  source("scripts/08_equations.r")
  setwd(root_dir)
}

# Launch Calculator (uses Calculator renv)
{
  setwd(file.path(root_dir, "Calculator"))
  renv::restore(prompt = FALSE)
  source("deploy/run_and_deploy.r")
  setwd(root_dir)
}

