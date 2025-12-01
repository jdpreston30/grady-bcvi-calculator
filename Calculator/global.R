# Load packages from DESCRIPTION file
# Packages are installed via: renv::restore()
desc_file <- file.path("DESCRIPTION")
if (file.exists(desc_file)) {
  desc_imports <- read.dcf(desc_file, fields = "Imports")[[1]]
  packages <- trimws(strsplit(desc_imports, ",")[[1]])
  invisible(lapply(packages, library, character.only = TRUE))
}

# Resolve namespace conflicts
conflicted::conflicts_prefer(gridtext::richtext_grob)
conflicted::conflicts_prefer(ggplot2::margin)
conflicted::conflicts_prefer(
  dplyr::filter,
  dplyr::select,
  dplyr::mutate
)

lasso_coefs <- readRDS("data/lasso_weighted_coefs.rds")
platt_model <- readRDS("data/platt_model.rds")

source("R/plotting/plot_asa_benefit_setup.R")
source("R/plotting/plot_asa_benefit.R")
source("R/plotting/plot_asa_benefit_mobile.R")
source("R/models/counterfactual_asa.R")