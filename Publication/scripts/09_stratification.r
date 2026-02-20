#* 9: Risk Stratification and Clinical Utility Metrics
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
#+ 9.7: Display and export table
#- 9.7.1: Print to console (with 2 decimal precision)
cat("\n=== Risk Stratification and Predictive Performance ===\n\n")
print(ST_risk_strata_display, n = Inf)
#- 9.7.2: Export to Excel (with rounded whole numbers)
write.xlsx(ST_risk_strata_excel, "Outputs/Tables/ST4.xlsx")
