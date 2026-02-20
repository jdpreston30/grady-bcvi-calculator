  #* 7: ASA Timing Table
#+ 7.1 Cleanup and create stratified data
asa_timing_raw <- read_excel(descriptive_merged, sheet = "ASA_timing") %>%
  mutate(
    AT_timing_bin = case_when(
      PTAT <= 24 ~ "Within 24h of Admission",
      PTAT <= 48 ~ "Within 48h of Admission",
      is.na(PTAT) ~ "Never Received AT",
      TRUE ~ "> 48h Following Admission"
    ),
    AT_timing_bin = factor(AT_timing_bin, levels = c(
      "Within 24h of Admission",
      "Within 48h of Admission",
      "> 48h Following Admission",
      "Never Received AT"
    )),
    dose_formatted = case_when(
      dose == "81 mg ASA" ~ "ASA (81 mg, PO)",
      dose == "325 mg ASA" ~ "ASA (325 mg, PO)",
      dose == "600 mg (suppository)" ~ "ASA (600 mg, PR)",
      dose == "Heparin gtt" ~ "Heparin (GTT)",
      dose == "None" ~ "Never Received AT",
      is.na(dose) ~ "Never Received AT",
      TRUE ~ as.character(dose)
    ),
    dose_formatted = factor(dose_formatted, levels = c(
      "ASA (81 mg, PO)",
      "ASA (325 mg, PO)",
      "ASA (600 mg, PR)",
      "Heparin (GTT)",
      "Never Received AT"
    ))
  ) %>%
  select(AT_prestroke, `Presentation to Stroke (h)` = PTS, `AT Initiation` = AT_timing_bin, `AT Choice` = dose_formatted)

#+ 7.2 Run stats on the variables with TernG
T3_i <- ternG(
  data = asa_timing_raw,
  group_var = "AT_prestroke",
  force_ordinal = "PTS",
  descriptive = TRUE,
  factor_order = "levels",
  round_intg = TRUE,
  category_start = c("Presentation to Stroke (h)" = "Event Timing", "AT Choice" = "AT Choice"),
)

#+ 7.3: Format tibble for export
T3 <- T3_i |>
  # Step 1: Update .indent values
  mutate(
    .indent = case_when(
      .indent == 2 ~ 3,
      row_number() >= 10 & row_number() <= 14 ~ 3,  # 81 mg ASA to 600 mg suppository rows
      TRUE ~ .indent
    )
  ) |>
  # Step 2: Add spaces to Variable column based on .indent
  mutate(
    Variable = case_when(
      is.na(.indent) ~ Variable,
      TRUE ~ paste0(strrep(" ", .indent), Variable)
    )
  ) |>
  # Step 3: Rename Variable column
  rename(`Category: Variable: Stratified Variable` = Variable) |>
  # Step 4: Programmatically rename group columns (handle actual newline or \n)
  rename_with(
    .fn = ~ {
      # Handle N column (with newline or space)
      .x <- gsub("^N\\s*[\n\r]*\\s*\\(n = (\\d+)\\)$", "AT Initiated Post-Stroke* (n = \\1)", .x)
      # Also handle literal \n in the string
      .x <- gsub("^N\\\\n\\(n = (\\d+)\\)$", "AT Initiated Post-Stroke* (n = \\1)", .x)
      .x
    }
  ) |>
  rename_with(
    .fn = ~ {
      # Handle Y column (with newline or space)
      .x <- gsub("^Y\\s*[\n\r]*\\s*\\(n = (\\d+)\\)$", "AT Initiated Pre-Stroke (n = \\1)", .x)
      # Also handle literal \n in the string
      .x <- gsub("^Y\\\\n\\(n = (\\d+)\\)$", "AT Initiated Pre-Stroke (n = \\1)", .x)
      .x
    }
  ) |>
  # Step 5: Replace newline or literal \n in Total column with space
  rename_with(
    .fn = ~ {
      .x <- gsub("Total[\n\r]\\(N = ", "Total (N = ", .x)
      .x <- gsub("Total\\\\n\\(N = ", "Total (N = ", .x)
      .x
    }
  ) |>
  # Step 6: Round p values to 2 decimals (preserve scientific notation for very small values)
  mutate(
    p = case_when(
      p == "-" ~ "-",
      p == "" ~ "",
      TRUE ~ {
        p_numeric <- suppressWarnings(as.numeric(p))
        if_else(
          is.na(p_numeric), 
          p, 
          if_else(p_numeric < 0.01, p, sprintf("%.2f", p_numeric))  # Keep scientific notation if < 0.01
        )
      }
    )
  ) |>
  # Step 7: Rename p column to add dagger
  rename(`p-value†` = p) |>
  # Step 8: Remove duplicate "   AT Choice" row (keep only "AT Choice" without spaces)
  filter(`Category: Variable: Stratified Variable` != "   AT Choice") |>
  # Step 9: Select final columns in specified order
  select(`Category: Variable: Stratified Variable`, 
         starts_with("AT Initiated Pre-Stroke"), 
         starts_with("AT Initiated Post-Stroke"), 
         `p-value†`, 
         starts_with("Total"))

#+ 7.4: View and export formatted table
cat("\n=== Table 3 (T3) ===\n")
print(T3, n = Inf)
write.xlsx(T3, "Outputs/Tables/T3.xlsx", rowNames = FALSE)
