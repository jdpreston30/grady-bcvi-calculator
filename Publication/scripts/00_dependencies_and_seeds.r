#* 0: Dependencies and setting seeds
#+ 0.1: Dependencies
#- 0.1.1: Load all packages from DESCRIPTION file
# Packages are installed via: renv::restore()
# This reads DESCRIPTION and loads all packages automatically
desc_file <- file.path("DESCRIPTION")
if (file.exists(desc_file)) {
  desc_imports <- read.dcf(desc_file, fields = "Imports")[[1]]
  packages <- trimws(strsplit(desc_imports, ",")[[1]])
  invisible(lapply(packages, library, character.only = TRUE))
}
#+ 0.2: Set seeds
set.seed(2025)
my_seeds_rf <- c(replicate(100, sample.int(1000, 5), simplify = FALSE), list(sample.int(1000, 1)))
#+ 0.3: Call all utility and modeling functions
purrr::walk(
  list.files(
    here::here("R"),  # Fixed: already in Publication directory
    pattern = "\\.[rR]$",
    full.names = TRUE,
    recursive = TRUE
  ) %>%
    purrr::keep(~ !grepl("/dependency_finder/", .x)) %>%
    purrr::keep(~ !grepl("99_compiled\\.r$", .x, ignore.case = TRUE)),
  source
)
#+ 0.4: Resolve conflicts
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
conflicts_prefer(stats::lag)