#' Deploy Calculator to shinyapps.io
#' Run from root directory or Calculator directory

# Set working directory to Calculator root
if (basename(getwd()) != "Calculator") {
  if (dir.exists("Calculator")) {
    setwd("Calculator")
  } else {
    stop("Must run from root directory or Calculator directory")
  }
}

# Load packages (from Calculator renv)
library(rsconnect)
library(shiny)

# Run shiny app locally
runApp()

# Deploy to shinyapps.io (uncomment to deploy)
setwd("Calculator")
renv::restore()
rsconnect::deployApp(
  appDir = ".",
  appName = "Calculator",
  account = "grady-bcvi-calc",
  forceUpdate = TRUE
)
