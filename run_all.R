# Save root directory
root_dir <- getwd()

# Publication Analysis Pipeline (uses Publication renv)
{
  # Set up working directory and restore Publication renv environment
  setwd(file.path(root_dir, "Publication"))
  renv::restore(prompt = FALSE)
  
  # Load dependencies, seeds, and auto-detect computer/data paths from config.yaml
  source("scripts/00_setup.r")  
  source("scripts/01_import_and_preprocess.r")
  source("scripts/02_descriptive_statistics.r")
  source("scripts/03_trad_linear_modeling.r")
  source("scripts/04_modeling_and_performance.r")
  source("scripts/05_ROC_construction.r")
  source("scripts/06_risk_simulations.r")
  source("scripts/07_asa_timing.r")
  source("scripts/08_equations.r")
  source("scripts/09_stratification.r")
  setwd(root_dir)
}

output <- capture.output({
  source("scripts/01_import_and_preprocess.r")
  source("scripts/02_descriptive_statistics.r")
  source("scripts/03_trad_linear_modeling.r")
})
writeLines(output, "output.txt")
# Launch Calculator (uses Calculator renv)
{
  setwd(file.path(root_dir, "Calculator"))
  renv::restore(prompt = FALSE)
  source("deploy/run_and_deploy.r")
  setwd(root_dir)
}
