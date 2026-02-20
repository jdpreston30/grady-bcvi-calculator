#* 9: Risk Stratification and Clinical Utility Metrics
#! Not ultimately included in manuscript, but providing here for context
#+ 9.1: Define risk strata thresholds
#! Strata: Low (<10%), Moderate (10-30%), High (≥30%)
#! Justification: Clinically interpretable cutpoints with clear separation in observed outcomes
risk_strata_thresholds <- c(0, 0.10, 0.30, 1.0)
risk_strata_labels <- c("Low (<10%)", "Moderate (10-30%)", "High (≥30%)")
#+ 9.2: Assign risk strata to each patient
stratification_df <- calibration_df %>% # From 06_risk_simulations.r
  mutate(
    risk_stratum = cut(platt_prob, 
                       breaks = risk_strata_thresholds,
                       labels = risk_strata_labels,
                       include.lowest = TRUE)
  )
#+ 9.3: Calculate metrics for each stratum
stratum_metrics <- stratification_df %>%
  group_by(risk_stratum) %>%
  summarise(
    n_patients = n(),
    n_strokes = sum(truth_bin),
    observed_rate = mean(truth_bin),
    .groups = "drop"
  ) %>%
  mutate(
    pct_of_total = n_patients / sum(n_patients) * 100
  )
#+ 9.4: Calculate PPV/NPV and Sensitivity/Specificity at each threshold
#+ 9.5: Calculate PPV/NPV and Sensitivity/Specificity at each threshold
#- For each threshold, calculate classification metrics
#- Note: Low stratum has no meaningful threshold (would be 0%), so set to NA
threshold_metrics <- tibble(
  risk_stratum = risk_strata_labels,
  threshold = c(NA, 0.10, 0.30)  # NA for Low stratum
) %>%
  rowwise() %>%
  mutate(
    # Classify as positive if predicted prob >= threshold
    # For NA threshold (Low stratum), set all metrics to NA
    tp = ifelse(is.na(threshold), NA_real_, 
                sum(stratification_df$platt_prob >= threshold & stratification_df$truth_bin == 1)),
    fp = ifelse(is.na(threshold), NA_real_,
                sum(stratification_df$platt_prob >= threshold & stratification_df$truth_bin == 0)),
    tn = ifelse(is.na(threshold), NA_real_,
                sum(stratification_df$platt_prob < threshold & stratification_df$truth_bin == 0)),
    fn = ifelse(is.na(threshold), NA_real_,
                sum(stratification_df$platt_prob < threshold & stratification_df$truth_bin == 1)),
    
    # Calculate metrics
    sensitivity = tp / (tp + fn),
    specificity = tn / (tn + fp),
    ppv = tp / (tp + fp),
    npv = tn / (tn + fn)
  ) %>%
  ungroup() %>%
  select(risk_stratum, sensitivity, specificity, ppv, npv)

#+ 9.5: Combine into comprehensive table
#- 9.5.0: Define rounding helper
#! Note: Using floor(x + 0.5) for standard rounding (3.5 -> 4, not banker's)
rounder <- function(x, digits = 0) floor(x * 10^digits + 0.5) / 10^digits
#- 9.5.1: Combine stratum metrics with threshold metrics
risk_stratification_table <- stratum_metrics %>%
  left_join(threshold_metrics, by = "risk_stratum") %>%
  mutate(
    # Combined observed strokes column: n (%)
    observed_display = sprintf("%d (%.2f%%)", n_strokes, observed_rate * 100),
    observed_excel = sprintf("%d (%.0f%%)", n_strokes, rounder(observed_rate * 100, 0)),
    # Format percentages with 2 decimals for console display
    pct_of_total_display = sprintf("%.2f%%", pct_of_total),
    sensitivity_display = ifelse(is.na(sensitivity), "-", sprintf("%.2f%%", sensitivity * 100)),
    specificity_display = ifelse(is.na(specificity), "-", sprintf("%.2f%%", specificity * 100)),
    ppv_display = ifelse(is.na(ppv), "-", sprintf("%.2f%%", ppv * 100)),
    npv_display = ifelse(is.na(npv), "-", sprintf("%.2f%%", npv * 100)),
    # Format percentages as whole numbers for Excel export
    pct_of_total_excel = sprintf("%.0f%%", rounder(pct_of_total, 0)),
    sensitivity_excel = ifelse(is.na(sensitivity), "-", sprintf("%.0f%%", rounder(sensitivity * 100, 0))),
    specificity_excel = ifelse(is.na(specificity), "-", sprintf("%.0f%%", rounder(specificity * 100, 0))),
    ppv_excel = ifelse(is.na(ppv), "-", sprintf("%.0f%%", rounder(ppv * 100, 0))),
    npv_excel = ifelse(is.na(npv), "-", sprintf("%.0f%%", rounder(npv * 100, 0)))
  )
#+ 9.6: Create display and export versions
#- 9.6.1: Console display table (2 decimal places)
ST_risk_strata_display <- risk_stratification_table %>%
  select(
    `Risk Stratum` = risk_stratum,
    `N Patients (%)` = n_patients,
    `Percent of Total` = pct_of_total_display,
    `Observed Strokes` = observed_display,
    `Sensitivity` = sensitivity_display,
    `Specificity` = specificity_display,
    `PPV` = ppv_display,
    `NPV` = npv_display
  ) %>%
  mutate(
    `N Patients (%)` = ifelse(
      `Risk Stratum` == "Overall Population",
      as.character(`N Patients (%)`),
      sprintf("%d (%s)", `N Patients (%)`, `Percent of Total`)
    )
  ) %>%
  select(-`Percent of Total`)
#- 9.6.2: Excel export table (rounded to whole numbers)
ST_risk_strata_excel <- risk_stratification_table %>%
  select(
    `Risk Stratum` = risk_stratum,
    `N Patients (%)` = n_patients,
    `Percent of Total` = pct_of_total_excel,
    `Observed Strokes` = observed_excel,
    `Sensitivity` = sensitivity_excel,
    `Specificity` = specificity_excel,
    `PPV` = ppv_excel,
    `NPV` = npv_excel
  ) %>%
  mutate(
    `N Patients (%)` = ifelse(
      `Risk Stratum` == "Overall Population",
      as.character(`N Patients (%)`),
      sprintf("%d (%s)", `N Patients (%)`, `Percent of Total`)
    )
  ) %>%
  select(-`Percent of Total`)
#+ 9.7: Display table
#- 9.7.1: Print to console (with 2 decimal precision)
cat("\n=== Risk Stratification and Predictive Performance ===\n\n")
print(ST_risk_strata_display, n = Inf)
#+ 9.8: COMPARISON - Data-Driven Tertile Approach
#- 9.8.1: Calculate tertile cutoffs
cat("\n\n=== COMPARISON: Data-Driven Tertile Approach ===\n\n")
tertile_cutoffs <- quantile(stratification_df$platt_prob, probs = c(0, 1/3, 2/3, 1))
cat(sprintf("Tertile cutoffs: %.1f%%, %.1f%%, %.1f%%\n\n", 
            tertile_cutoffs[2] * 100, 
            tertile_cutoffs[3] * 100, 
            tertile_cutoffs[4] * 100))
#- 9.8.2: Assign tertile-based strata
tertile_labels <- c(
  sprintf("Low (<%0.1f%%)", tertile_cutoffs[2] * 100),
  sprintf("Moderate (%0.1f-%0.1f%%)", tertile_cutoffs[2] * 100, tertile_cutoffs[3] * 100),
  sprintf("High (≥%0.1f%%)", tertile_cutoffs[3] * 100)
)
#- 9.8.3: Make tertile-based dataframe
tertile_df <- stratification_df %>%
  mutate(
    risk_stratum = cut(platt_prob, 
                       breaks = tertile_cutoffs,
                       labels = tertile_labels,
                       include.lowest = TRUE)
  )
#- 9.8.4: Calculate metrics for each tertile
tertile_metrics <- tertile_df %>%
  group_by(risk_stratum) %>%
  summarise(
    n_patients = n(),
    n_strokes = sum(truth_bin),
    observed_rate = mean(truth_bin),
    .groups = "drop"
  ) %>%
  mutate(
    pct_of_total = n_patients / sum(n_patients) * 100
  )
#- 9.8.5: Calculate threshold metrics for tertiles
tertile_threshold_metrics <- tibble(
  risk_stratum = tertile_labels,
  threshold = c(NA, tertile_cutoffs[2], tertile_cutoffs[3])
) %>%
  rowwise() %>%
  mutate(
    tp = ifelse(is.na(threshold), NA_real_, 
                sum(tertile_df$platt_prob >= threshold & tertile_df$truth_bin == 1)),
    fp = ifelse(is.na(threshold), NA_real_,
                sum(tertile_df$platt_prob >= threshold & tertile_df$truth_bin == 0)),
    tn = ifelse(is.na(threshold), NA_real_,
                sum(tertile_df$platt_prob < threshold & tertile_df$truth_bin == 0)),
    fn = ifelse(is.na(threshold), NA_real_,
                sum(tertile_df$platt_prob < threshold & tertile_df$truth_bin == 1)),
    
    sensitivity = tp / (tp + fn),
    specificity = tn / (tn + fp),
    ppv = tp / (tp + fp),
    npv = tn / (tn + fn)
  ) %>%
  ungroup() %>%
  select(risk_stratum, sensitivity, specificity, ppv, npv)
#- 9.8.6: Combine and format tertile table
#- 9.8.6.1: Display version (2 decimals)
tertile_table <- tertile_metrics %>%
  left_join(tertile_threshold_metrics, by = "risk_stratum") %>%
  mutate(
    observed_display = sprintf("%d (%.2f%%)", n_strokes, observed_rate * 100),
    pct_of_total_display = sprintf("%.2f%%", pct_of_total),
    sensitivity_display = ifelse(is.na(sensitivity), "-", sprintf("%.2f%%", sensitivity * 100)),
    specificity_display = ifelse(is.na(specificity), "-", sprintf("%.2f%%", specificity * 100)),
    ppv_display = ifelse(is.na(ppv), "-", sprintf("%.2f%%", ppv * 100)),
    npv_display = ifelse(is.na(npv), "-", sprintf("%.2f%%", npv * 100))
  ) %>%
  mutate(
    `N Patients (%)` = sprintf("%d (%s)", n_patients, pct_of_total_display)
  ) %>%
  select(
    `Risk Stratum` = risk_stratum,
    `N Patients (%)`,
    `Observed Strokes` = observed_display,
    `Sensitivity` = sensitivity_display,
    `Specificity` = specificity_display,
    `PPV` = ppv_display,
    `NPV` = npv_display
  )
#- 9.8.6.2: Excel version (rounded whole numbers)
tertile_table_excel <- tertile_metrics %>%
  left_join(tertile_threshold_metrics, by = "risk_stratum") %>%
  mutate(
    observed_excel = sprintf("%d (%.0f%%)", n_strokes, rounder(observed_rate * 100, 0)),
    pct_of_total_excel = sprintf("%.0f%%", rounder(pct_of_total, 0)),
    sensitivity_excel = ifelse(is.na(sensitivity), "-", sprintf("%.0f%%", rounder(sensitivity * 100, 0))),
    specificity_excel = ifelse(is.na(specificity), "-", sprintf("%.0f%%", rounder(specificity * 100, 0))),
    ppv_excel = ifelse(is.na(ppv), "-", sprintf("%.0f%%", rounder(ppv * 100, 0))),
    npv_excel = ifelse(is.na(npv), "-", sprintf("%.0f%%", rounder(npv * 100, 0)))
  ) %>%
  mutate(
    `N Patients (%)` = sprintf("%d (%s)", n_patients, pct_of_total_excel)
  ) %>%
  select(
    `Risk Stratum` = risk_stratum,
    `N Patients (%)`,
    `Observed Strokes` = observed_excel,
    `Sensitivity` = sensitivity_excel,
    `Specificity` = specificity_excel,
    `PPV` = ppv_excel,
    `NPV` = npv_excel
  )
#- 9.8.7: Print tertile comparison
cat("=== Tertile-Based Risk Stratification ===\n")
cat("(Equal sample sizes per stratum)\n\n")
print(tertile_table, n = Inf)
#+ 9.9: Combine both approaches and export for reference
#- 9.9.1: Add approach identifier to clinical table (using rounded excel version)
clinical_export <- ST_risk_strata_excel %>%
  mutate(`Stratum Approach` = "Clinical", .before = 1)
#- 9.9.2: Add approach identifier to tertile table (using rounded excel version)
tertile_export <- tertile_table_excel %>%
  mutate(`Stratum Approach` = "Tertile", .before = 1)
#- 9.9.3: Combine, view, and export
threshold_comparison <- bind_rows(clinical_export, tertile_export)
cat("\n=== Threshold Comparison (not shown in manuscript) ===\n")
print(threshold_comparison, n = Inf)
write.xlsx(threshold_comparison, "Outputs/Tables/thresholds_not_shown.xlsx")
