#' Load Raw Data Paths
#'
#' Configure data file paths based on computer/environment
#'
#' @param which_computer Character: "laptop", "desktop", or "other"
#' @return Named list with raw_path and descriptive_merged paths
#' @export
load_raw_data <- function(which_computer = "desktop") {
  
  if (which_computer == "laptop") {
    raw_path <- "/Users/jdp2019/Library/CloudStorage/OneDrive-Emory/Research/Manuscripts and Projects/Grady/Risk Calculator/Raw Data/merged_data_DI.xlsx"
    descriptive_merged <- "/Users/jdp2019/Library/CloudStorage/OneDrive-Emory/Research/Manuscripts and Projects/Grady/Risk Calculator/Raw Data/descriptive_merged.xlsx"
    
  } else if (which_computer == "desktop") {
    raw_path <- "/Users/JoshsMacbook2015/Library/CloudStorage/OneDrive-EmoryUniversity/Research/Manuscripts and Projects/Grady/Risk Calculator/Raw Data/merged_data_DI.xlsx"
    descriptive_merged <- "/Users/JoshsMacbook2015/Library/CloudStorage/OneDrive-EmoryUniversity/Research/Manuscripts and Projects/Grady/Risk Calculator/Raw Data/descriptive_merged.xlsx"
    
  } else if (which_computer == "other") {
    cat("\n")
    cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
    cat("  CUSTOM DATA PATH CONFIGURATION\n")
    cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
    
    cat("Please provide the full path to your data files.\n")
    cat("Tip: You can drag and drop files into the terminal to paste their paths.\n\n")
    
    cat("Enter path to merged_data_DI.xlsx:\n")
    raw_path <- readline("> ")
    raw_path <- gsub("^['\"]|['\"]$", "", raw_path)  # Remove quotes if present
    raw_path <- trimws(raw_path)
    
    cat("\nEnter path to descriptive_merged.xlsx:\n")
    descriptive_merged <- readline("> ")
    descriptive_merged <- gsub("^['\"]|['\"]$", "", descriptive_merged)
    descriptive_merged <- trimws(descriptive_merged)
    
    cat("\n")
    cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
    cat("Paths configured:\n")
    cat("  Raw data: ", raw_path, "\n")
    cat("  Descriptive: ", descriptive_merged, "\n")
    cat("-" %>% rep(70) %>% paste(collapse = ""), "\n\n")
    
    # Validate paths exist
    if (!file.exists(raw_path)) {
      stop("Error: Raw data file not found at specified path:\n  ", raw_path)
    }
    if (!file.exists(descriptive_merged)) {
      stop("Error: Descriptive merged file not found at specified path:\n  ", descriptive_merged)
    }
    
  } else {
    stop("Invalid 'which_computer' value. Must be 'laptop', 'desktop', or 'other'")
  }
  
  # Return paths as a list and assign to global environment
  paths <- list(
    raw_path = raw_path,
    descriptive_merged = descriptive_merged
  )
  
  # Assign to global environment so scripts can access them
  assign("raw_path", raw_path, envir = .GlobalEnv)
  assign("descriptive_merged", descriptive_merged, envir = .GlobalEnv)
  
  invisible(paths)
}
