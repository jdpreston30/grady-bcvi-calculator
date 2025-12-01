  #* 7: ASA Timing Table
    #+ 7.1 Cleanup and create stratified data
      asa_timing_raw <- read_excel(descriptive_merged, sheet = "ASA_timing") %>%
        mutate(
          AT_timing_bin = case_when(
            PTAT <= 24 ~ "≤24h",
            PTAT <= 48 ~ "≤48h",
            is.na(PTAT) ~ "No ASA_AC", # For one non-ASA pt.
            TRUE ~ ">48h"
          )
        ) %>%
        select(AT_prestroke, PTS, AT_timing_bin, dose)
    #+ 7.1 Run stats on the variables with TernG
      T3 <- ternG(
        data = asa_timing_raw,
        group_var = "AT_prestroke",
        force_ordinal = "PTS",
        descriptive = TRUE,
        output_xlsx = "T3.xlsx",
        OR_col = TRUE
      )
