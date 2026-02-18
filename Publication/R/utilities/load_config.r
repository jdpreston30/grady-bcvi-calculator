#' Load Configuration and Set Up Data Paths
#'
#' Loads config.yaml from root directory, auto-detects the computer environment
#' (laptop vs desktop), and sets up global data file paths. Verifies all required
#' data files exist before returning.
#'
#' @param custom_data_path Optional. Custom path to data directory. If provided,
#'   auto-detection is skipped and this path is used directly.
#' @return List containing configuration with resolved paths and detected computer info
#' @export
load_config_and_paths <- function(custom_data_path = NULL) {
  # Load config from root directory
  config <- yaml::read_yaml(here::here("config.yaml"))
  
  # Use custom path if provided
  if (!is.null(custom_data_path)) {
    message("🔧 Using custom data path: ", custom_data_path)
    data_path <- custom_data_path
    detected_computer <- "custom"
  } else {
    # Auto-detect computer
    current_user <- Sys.getenv("USER")
    computer_name <- Sys.info()["nodename"]
    
    detected_computer <- NULL
    if (current_user == "jdp2019") {
      detected_computer <- "laptop"
      message("🔍 Detected laptop via username: ", current_user)
    } else if (current_user == "JoshsMacbook2015" || grepl("JoshsMacbook", computer_name, ignore.case = TRUE)) {
      detected_computer <- "desktop"
      message("🔍 Detected desktop via username/computer name: ", current_user)
    } else if (dir.exists("/Users/jdp2019")) {
      detected_computer <- "laptop"
      message("🔍 Detected laptop via path signature")
    } else if (dir.exists("/Users/JoshsMacbook2015")) {
      detected_computer <- "desktop"
      message("🔍 Detected desktop via path signature")
    } else {
      stop("Could not auto-detect computer. Current user: ", current_user, ", Computer: ", computer_name,
           "\n\nPlease either:",
           "\n  1. Add your computer to config.yaml under 'computers:', OR",
           "\n  2. Set custom path: load_config_and_paths(custom_data_path = '/path/to/data')")
    }
    
    message("🖥️ Using configuration for: ", detected_computer)
    
    # Extract paths for detected computer
    computer_config <- config$computers[[detected_computer]]
    data_path <- computer_config$data_path
  }
  
  # Set global data paths in parent environment
  assign("raw_path", file.path(data_path, config$data_files$raw_path), envir = .GlobalEnv)
  assign("descriptive_merged", file.path(data_path, config$data_files$descriptive_merged), envir = .GlobalEnv)
  
  # Verify files exist
  if (!file.exists(raw_path)) stop("Data file not found: ", raw_path)
  if (!file.exists(descriptive_merged)) stop("Data file not found: ", descriptive_merged)
  
  message("✓ All data files found")
  
  # Return config with metadata
  config$detected_computer <- detected_computer
  invisible(config)
}
