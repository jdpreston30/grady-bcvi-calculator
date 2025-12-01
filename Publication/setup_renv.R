# Setup renv for Publication folder
# This script initializes renv and captures all dependencies including GitHub packages

# Initialize renv
if (!require("renv", quietly = TRUE)) {
  install.packages("renv", repos = "https://cloud.r-project.org")
}

# Initialize bare renv project
renv::init(bare = TRUE)

# Install TernTablesR from GitHub first
if (!require("remotes", quietly = TRUE)) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

cat("Installing TernTablesR from GitHub...\n")
remotes::install_github("jdpreston30/TernTablesR", upgrade = "never")

# Now snapshot to capture it in renv.lock
cat("Creating renv.lock snapshot...\n")
renv::snapshot(prompt = FALSE)

cat("✓ renv setup complete for Publication\n")
cat("✓ TernTablesR installed from GitHub\n")
cat("✓ renv.lock created\n")
