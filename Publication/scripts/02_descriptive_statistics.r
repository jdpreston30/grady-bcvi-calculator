#* 2: Descriptive Statistics
#+ 2.1: Convert them all to factors with levels "N" and "Y"
descriptive_data <- raw_modeling_i %>%
  mutate(across(
    all_of(c(
      "stroke", "ASA", "sexM", "BLC", "BLV",
      "MFC_present", "MFV_present",
      "isolated_C", "isolated_V", "concom_CV"
    )),
    ~ factor(if_else(. == 1, "Y", "N"), levels = c("N", "Y"))
  )) %>%
  arrange(desc(stroke))
#+ 2.2: Compute cohort descriptive statistics
#- 2.2.1: Bring in other descriptive data from above
descriptive_data_join_1 <- descriptive_data %>%
  select(stroke:BLV, isolated_C, isolated_V, concom_CV) %>%
  mutate(
    max_vert = na_if(max_vert, 0),
    max_carotid = na_if(max_carotid, 0)
  ) %>%
  mutate(
    BLC = case_when(
      isolated_C == "Y" | concom_CV == "Y" ~ as.character(BLC),
      TRUE ~ NA_character_
    ),
    BLV = case_when(
      isolated_V == "Y" | concom_CV == "Y" ~ as.character(BLV),
      TRUE ~ NA_character_
    )
  ) %>%
    mutate(
      BLC = factor(BLC, levels = c("N", "Y")),
      BLV = factor(BLV, levels = c("N", "Y"))
    )
#- 2.2.2: Bring in descriptive data from registry and simplify CC
#! NS238, listed as 'MVC, Pedestrian', injury was MVC -> peds v auto, categorizing as Other or NS
# ! NS389, listed as 'Motorcycle, Pedestrian', injury was MCC -> peds v auto, categorizing as Other or NS
# ! NS504, listed as 'Motorcycle, Pedestrian', injury was Four Wheeler v Car, categorizing as MVA
# ! NS132, listed as 'MVC, fall Under 1m (3.3 ft)'', injury was MVC -> tased and GLF, categorizing as MVA
# ! NS477 listed as 'MVC, fall Under 1m (3.3 ft)'', injury was MVC -> GLF from stretcher, categorizing as MVA
# ! NS748, listed as 'Other Motorized Vehicle, Fall Under 1m (', injury was fall from golf cart, categorizing as FFH <1m
descriptive_data_join_2 <- read_excel(descriptive_merged, sheet = "merge") %>%
  mutate(chief_complaint = case_when(
    ID == "NS238" ~ "Other or NS",
    ID == "NS389" ~ "Other or NS",
    ID == "NS504" ~ "MVA",
    ID == "NS132" ~ "MVA",
    ID == "NS477" ~ "MVA",
    ID == "NS748" ~ "GLF",
    TRUE ~ chief_complaint # retain original values for all others
  )) %>%
  mutate(CC_simplified = recode(
    chief_complaint,
    "Assault, Not Applicable" = "Assault",
    "Other Blunt Mechanism, Assault" = "Assault",
    "Assault, Other Blunt Mechanism" = "Assault",
    "Assault, Other Burn Mechanism" = "Assault",
    "Assault, Other Penetrating Mechanism" = "Assault",
    "Bicycle" = "Bicycle",
    "Bicycle, Not Applicable" = "Bicycle",
    "Fall - NFS" = "Other or NS",
    "Fall - NFS, Not Applicable" = "Other or NS",
    "Assault, Fall 1m - 6m (3.3 - 19.7 ft)" = "FFH",
    "Fall 1m - 6m (3.3 - 19.7 ft)" = "FFH",
    "Fall Over 6m (19.7 ft)" = "FFH",
    "Fall Under 1m (3.3 ft)" = "GLF",
    "Fall 1m - 6m (3.3 - 19.7 ft), Electrical" = "FFH",
    "Fall 1m - 6m (3.3 - 19.7 ft), Not Applic" = "FFH",
    "Fall 1m - 6m (3.3 - 19.7 ft), Other Blun" = "FFH",
    "Other Blunt Mechanism, Fall 1m - 6m (3.3" = "FFH",
    "Fall Over 6m (19.7 ft), Not Applicable" = "FFH",
    "Fall Under 1m (3.3 ft), Not Applicable" = "GLF",
    "Fall Under 1m (3.3 ft), Pedestrian" = "GLF",
    "Motorcycle" = "MCC",
    "Motorcycle, Not Applicable" = "MCC",
    "Motorcycle, Unknown" = "MCC",
    "MVC" = "MVA",
    "MVC, MVC" = "MVA",
    "MVC, Not Applicable" = "MVA",
    "MVC, Other Blunt Mechanism" = "MVA",
    "Not Applicable" = "Other or NS",
    "Other Blunt Mechanism" = "Other or NS",
    "Other Blunt Mechanism, Not Applicable" = "Other or NS",
    "Other Motorized Vehicle" = "MVA",
    "Pedestrian" = "PvA",
    "Pedestrian, Not Applicable" = "PvA"
  )) %>%
  select(-c(Dataset, chief_complaint))
#- 2.2.3: Bring in total injury counts data, compute totals to join later
#_Specify carotid and vertebral vectors
cols_C <- c(cols_LC, cols_RC)
cols_V <- c(cols_LV, cols_RV, cols_VB)
#_Now get injury counts
injury_counts <- raw %>%
  rowwise() %>%
  mutate(
    Cg1 = sum(c_across(all_of(cols_C)) == 1, na.rm = TRUE),
    Cg2 = sum(c_across(all_of(cols_C)) == 2, na.rm = TRUE),
    Cg3 = sum(c_across(all_of(cols_C)) == 3, na.rm = TRUE),
    Cg4 = sum(c_across(all_of(cols_C)) == 4, na.rm = TRUE),
    Cg5 = sum(c_across(all_of(cols_C)) == 5, na.rm = TRUE),
    Vg1 = sum(c_across(all_of(cols_V)) == 1, na.rm = TRUE),
    Vg2 = sum(c_across(all_of(cols_V)) == 2, na.rm = TRUE),
    Vg3 = sum(c_across(all_of(cols_V)) == 3, na.rm = TRUE),
    Vg4 = sum(c_across(all_of(cols_V)) == 4, na.rm = TRUE),
    Vg5 = sum(c_across(all_of(cols_V)) == 5, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(ID, injury_complex, Cg1, Cg2, Cg3, Cg4, Cg5, Vg1, Vg2, Vg3, Vg4, Vg5)
#_Now make a summary to join with main later
injury_summary <- injury_counts %>%
  select(Cg1:Cg5, Vg1:Vg5) %>%
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(everything(), names_to = "Grade", values_to = "n") %>%
  mutate(
    Vessel = case_when(
      str_starts(Grade, "C") ~ "Carotid",
      str_starts(Grade, "V") ~ "Vertebral"
    )
  ) %>%
  group_by(Vessel) %>%
  mutate(
    pct = round(n / sum(n) * 100, 0),
    pct_display = if_else(n > 0 & pct == 0, "<1", as.character(pct)),
    Value = paste0(n, " (", pct_display, "%)"),
    Grade = stringr::str_remove(Grade, "^[CV]g"),
    Grade_roman = case_when(
      Grade == "1" ~ "I",
      Grade == "2" ~ "II",
      Grade == "3" ~ "III",
      Grade == "4" ~ "IV",
      Grade == "5" ~ "V"
    ),
    Variable = paste0("      Biffl ", Grade_roman)
  ) %>%
  ungroup() %>%
  select(Variable, Value, Vessel) %>%
  # Add header rows with totals
  group_by(Vessel) %>%
  group_modify(~ {
    # Calculate total for this vessel
    total_n <- sum(str_extract(.x$Value, "^[0-9]+") %>% as.numeric())
    total_pct <- round(total_n / sum(
      injury_counts$Cg1, injury_counts$Cg2, injury_counts$Cg3, injury_counts$Cg4, injury_counts$Cg5,
      injury_counts$Vg1, injury_counts$Vg2, injury_counts$Vg3, injury_counts$Vg4, injury_counts$Vg5
    ) * 100, 0)
    header <- tibble(
      Variable = paste0("  ", unique(.y$Vessel), " BCVIs*"),
      Value = paste0(total_n, " (", total_pct, "%)")
    )
    bind_rows(header, .x %>% select(Variable, Value))
  }) %>%
  ungroup() %>%
  select(Variable, Value)
#- 2.2.4: Join with original raw import, arrange
full_descriptive <- raw_iiii %>%
  select(ID,DC_status) %>%
  left_join(descriptive_data_join_1, by = "ID") %>%
  left_join(descriptive_data_join_2, by = "ID") %>%
  select(
    ID, age, BMI, sexM, race, # Demographics
    CC_simplified, SBP, DBP, HR, GCS, ISS, AIS_head, AIS_face, AIS_neck, AIS_spine, # Clinical Features
    BLC, BLV, max_carotid, max_vert, isolated_C, isolated_V, concom_CV, # Injury pattern
    stroke, ASA, DC_status, ICU_days, vent_days, THD, # Clinical Course and Outcomes
  ) %>%
  mutate(
    ICU_days = if_else(DC_status == "Dead", NA_real_, ICU_days),
    vent_days = if_else(DC_status == "Dead", NA_real_, vent_days),
    THD = if_else(DC_status == "Dead", NA_real_, THD)
  )
#+ 2.3: Create Table 1 (Descriptive Statistics)
#- 2.3.1: Structure data for T1 with columns ordered per desired presentation
T1_data <- full_descriptive %>%
  select(
    ID,
    # Demographics
    "Age (yr)" = age, 
    BMI, 
    "Sex (Male)" = sexM, 
    Race = race,
    # Vitals
    "Systolic Blood Pressure" = SBP, 
    "Diastolic Blood Pressure" = DBP, 
    "Heart Rate" = HR,
    # Mechanism
    "Mechanism of Injury" = CC_simplified,
    # Injury Severity
    GCS, 
    ISS, 
    "AIS Head" = AIS_head, 
    "AIS Neck" = AIS_neck, 
    "AIS Spine" = AIS_spine, 
    "AIS Face" = AIS_face,
    # BCVI Details
    max_carotid, 
    max_vert,
    `Bilateral Carotid` = BLC, 
    `Bilateral Vertebral` = BLV,
    "Isolated Carotid" = isolated_C, 
    "Isolated Vertebral" = isolated_V, 
    "Concomitant C/V" = concom_CV,
    # Outcomes
    Stroke = stroke, 
    `Given Antithrombotic` = ASA, 
    `Survival` = DC_status,
    `ICU LOS (d)†` = ICU_days, 
    `Ventilator Days†` = vent_days, 
    `Hospital LOS (d)†` = THD
  ) %>%
  mutate(
    # Recode and reorder Mechanism of Injury
    `Mechanism of Injury` = case_when(
      `Mechanism of Injury` == "MVA" ~ "Motor Vehicle Collision",
      `Mechanism of Injury` == "GLF" ~ "Ground Level Fall",
      `Mechanism of Injury` == "PvA" ~ "Pedestrian Vs. Auto",
      `Mechanism of Injury` == "FFH" ~ "Fall from Height",
      `Mechanism of Injury` == "MCC" ~ "Motorcycle Collision",
      `Mechanism of Injury` == "Bicycle" ~ "Bicycle Accident",
      `Mechanism of Injury` == "Assault" ~ "Assault",
      `Mechanism of Injury` == "Other or NS" ~ "Other/Not Specified",
      TRUE ~ `Mechanism of Injury`
    ),
    `Mechanism of Injury` = factor(
      `Mechanism of Injury`,
      levels = c(
        "Motor Vehicle Collision",
        "Ground Level Fall",
        "Pedestrian Vs. Auto",
        "Fall from Height",
        "Motorcycle Collision",
        "Bicycle Accident",
        "Assault",
        "Other/Not Specified"
      )
    ),
    # Reorder Race factor
    Race = factor(Race, levels = c("Black", "White", "Other or Unknown")),
    # Convert Survival to Y/N factor
    Survival = factor(
      if_else(Survival == "Alive", "Y", "N"),
      levels = c("N", "Y")
    )
  )
#- 2.3.2: Run ternD to get descriptive statistics with injury summary rows added in
T1 <- ternD(
  data = T1_data,
  force_ordinal = c("GCS", "ISS", "AIS Head", "AIS Neck", "AIS Spine", "AIS Face", "ICU LOS (d)†", "Ventilator Days†", "Hospital LOS (d)†"),
  exclude_vars = c("ID", "max_carotid", "max_vert"),
  round_intg = TRUE,
  factor_order = "levels"
) %>%
  mutate(rownum = row_number()) %>%
  {
    # Find insertion point after AIS Face
    ais_face_idx <- grep("AIS Face", .$Variable, ignore.case = TRUE)
    
    # Get injury summary and rename Value column to match ternD output
    col_name <- setdiff(names(.), c("Variable", "rownum"))[1]
    
    injury_rows <- injury_summary %>%
      rename(!!col_name := Value) %>%
      mutate(rownum = NA_real_)
    
    # Insert injury summaries after AIS Face
    bind_rows(
      dplyr::slice(., 1:ais_face_idx),
      injury_rows,
      dplyr::slice(., (ais_face_idx + 1):n())
    )
  } %>%
  select(-rownum) %>%
  # Add custom grouping headers and format
  mutate(rownum = row_number()) %>%
  {
    # Find rows for grouping
    age_idx <- which(.$Variable == "  Age (yr)")
    race_other_idx <- which(.$Variable == "      Other or Unknown")
    sbp_idx <- which(.$Variable == "  Systolic Blood Pressure")
    gcs_idx <- which(.$Variable == "  GCS")
    ais_face_idx <- which(grepl("AIS Face", .$Variable))[length(which(grepl("AIS Face", .$Variable)))]
    carotid_bcvi_idx <- which(.$Variable == "  Carotid BCVIs*")
    bilateral_carotid_idx <- which(.$Variable == "  Bilateral Carotid")
    isolated_carotid_idx <- which(.$Variable == "  Isolated Carotid")
    stroke_idx <- which(.$Variable == "  Stroke")
    
    # Create section and grouping headers
    demographics_header <- tibble(Variable = "Demographics", Summary = "", rownum = NA_real_)
    blank_row <- tibble(Variable = NA_character_, Summary = NA_character_, rownum = NA_real_)
    clinical_features_header <- tibble(Variable = "Clinical Features", Summary = "", rownum = NA_real_)
    injury_severity_header <- tibble(Variable = "  Injury Severity", Summary = "", rownum = NA_real_)
    injury_characteristics_header <- tibble(Variable = "Injury Characteristics", Summary = "", rownum = NA_real_)
    bilateral_injury_header <- tibble(Variable = "  Bilateral Injury", Summary = "", rownum = NA_real_)
    injury_complex_header <- tibble(Variable = "  Injury Complex", Summary = "", rownum = NA_real_)
    clinical_outcomes_header <- tibble(Variable = "Clinical Course and Outcomes", Summary = "", rownum = NA_real_)
    
    # Build the modified table
    result <- bind_rows(
      # Demographics section
      demographics_header,
      dplyr::slice(., age_idx:race_other_idx),
      blank_row,
      # Clinical Features section
      clinical_features_header,
      dplyr::slice(., sbp_idx:(gcs_idx - 1)),
      # Injury Severity with indented rows
      injury_severity_header,
      dplyr::slice(., gcs_idx:(gcs_idx + 5)) %>%
        mutate(Variable = paste0("  ", Variable)),
      blank_row,
      # Injury Characteristics section
      injury_characteristics_header,
      dplyr::slice(., carotid_bcvi_idx:(bilateral_carotid_idx - 1)),
      # Bilateral Injury with renamed and indented rows
      bilateral_injury_header,
      dplyr::slice(., bilateral_carotid_idx) %>%
        mutate(Variable = "      Carotid"),
      dplyr::slice(., bilateral_carotid_idx + 1) %>%
        mutate(Variable = "      Vertebral"),
      # Injury Complex with indented rows
      injury_complex_header,
      dplyr::slice(., isolated_carotid_idx:(isolated_carotid_idx + 2)) %>%
        mutate(Variable = paste0("  ", Variable)),
      blank_row,
      # Clinical Course and Outcomes section
      clinical_outcomes_header,
      dplyr::slice(., stroke_idx:n())
    )
    result
  } %>%
  select(-rownum)
#- 2.3.3: Export as excel sheet
write.xlsx(T1, "Outputs/Tables/T1.xlsx")
#+ 2.4: Run ternG on only the carotid injuries
#- 2.4.1: Filter to carotid and vertebral
{
  carotid_only <- descriptive_data %>% filter(isolated_C == "Y" | concom_CV == "Y")
  vertebral_only <- descriptive_data %>% filter(isolated_V == "Y" | concom_CV == "Y")
}
#- 2.4.2: Create a function to make stroke counts for whole population
stroke_counts <- function(df, label) {
  tab <- df %>%
    count(stroke) %>%
    tidyr::pivot_wider(names_from = stroke, values_from = n)

  n_N <- tab$N
  n_Y <- tab$Y
  total <- n_N + n_Y

  tibble(
    Variable = label,
    N = sprintf("%d", n_N),
    Y = sprintf("%d", n_Y),
    p = NA_character_,
    test = NA_character_,
    OR = NA_character_
  )
}
#- 2.4.3: Compute combined descriptive stats for totals
combined_descriptives <- bind_rows(
  stroke_counts(descriptive_data, "All Patients"),
  stroke_counts(carotid_only, "Carotid Patients"),
  stroke_counts(vertebral_only, "Vertebral Patients")
) %>%
  mutate(
    Vessel = case_when(
      Variable == "Carotid Patients" ~ "Carotid",
      Variable == "Vertebral Patients" ~ "Vertebral",
      TRUE ~ "ALL"
    ),
    # Calculate Total column
    Total = sprintf("%d", 
      as.numeric(str_extract(N, "^[0-9]+")) + as.numeric(str_extract(Y, "^[0-9]+"))),
    # Create consolidated OR/p-value column (will be NA for these summary rows)
    `OR [95% CI] or p-value*` = NA_character_
  ) %>%
  # Reorder columns to match ternG output
  select(Variable, N, Y, Total, `OR [95% CI] or p-value*`, Vessel)
#- 2.4.4: Run ternG for carotid results
carotid_results <- ternG(
  data = carotid_only,
  vars = c("BLC", "no_MFC", "MFC_present", "max_carotid", "concom_CV","carotid_segments","tot_carotid_inj","ISS","GCS"),
  group_var = "stroke",
  force_ordinal = c("max_carotid", "no_MFC", "ISS", "GCS"),
  descriptive = TRUE,
  output_docx = NULL,
  OR_col = TRUE,
  round_intg = TRUE
) %>%
  select(Variable = 1, N = 2, Y = 3, Total = 4, p = 5, test = 7, OR = 6) %>%
  mutate(
    Vessel = "Carotid",
    # Create consolidated OR/p-value column: use OR for categorical (%), p for ordinal [IQR]
    `OR [95% CI] or p-value*` = ifelse(str_detect(N, "%"), OR, p)
  ) %>%
  select(Variable, N, Y, Total, `OR [95% CI] or p-value*`, Vessel)
#- 2.4.5: Run ternG for vertebral results
vertebral_results <- ternG(
  data = vertebral_only,
  vars = c("BLV", "no_MFV", "MFV_present", "max_vert", "concom_CV","vertebral_segments","tot_vert_inj","ISS", "GCS"),
  group_var = "stroke",
  force_ordinal = c("max_vert", "no_MFV", "ISS", "GCS"),
  descriptive = TRUE,
  output_docx = NULL,
  OR_col = TRUE,
  round_intg = TRUE
) %>%
  select(Variable = 1, N = 2, Y = 3, Total = 4, p = 5, test = 7, OR = 6) %>%
  mutate(
    Vessel = "Vertebral",
    # Create consolidated OR/p-value column: use OR for categorical (%), p for ordinal [IQR]
    `OR [95% CI] or p-value*` = ifelse(str_detect(N, "%"), OR, p)
  ) %>%
  select(Variable, N, Y, Total, `OR [95% CI] or p-value*`, Vessel)
#- 2.4.6: Get patient counts
{
  vert_patients <- combined_descriptives %>% filter(Variable == "Vertebral Patients")
  carotid_patients <- combined_descriptives %>% filter(Variable == "Carotid Patients")
  all_patients <- combined_descriptives %>% filter(Variable == "All Patients")
}
#- 2.4.7: Extract counts for column headers
{
  all_n <- as.numeric(str_extract(all_patients$N, "^[0-9]+"))
  all_y <- as.numeric(str_extract(all_patients$Y, "^[0-9]+"))
  all_total <- all_n + all_y
  all_n_pct <- round(all_n / all_total * 100)
  all_y_pct <- round(all_y / all_total * 100)
}
#- 2.4.8: Combine results - simplified approach with proper ordering and headers
combined_results_raw <- bind_rows(
  combined_descriptives,
  carotid_results,
  vertebral_results
) %>%
  filter(Variable != "All Patients")  # Remove All Patients row after extracting values
#- 2.4.9: Insert a vertebral section header
vertebral_section <- combined_results_raw %>% 
  filter(Vessel == "Vertebral") %>%
  filter(Variable %in% c("Vertebral Patients", "  MFV_present", "  BLV", "  concom_CV", "  max_vert", "  ISS", "  GCS")) %>%
  mutate(Variable = case_when(
    Variable == "Vertebral Patients" ~ "  Number of Patients",
    Variable == "  MFV_present" ~ "  Multifocal Injury",
    Variable == "  BLV" ~ "  Bilateral Injury",
    Variable == "  concom_CV" ~ "  Concomitant CBCVI",
    Variable == "  max_vert" ~ "  Median Grade (IQR)†",
    TRUE ~ Variable
  )) %>%
  mutate(sort_order = case_when(
    Variable == "  Number of Patients" ~ 1,
    Variable == "  Multifocal Injury" ~ 2,
    Variable == "  Bilateral Injury" ~ 3,
    Variable == "  Concomitant CBCVI" ~ 4,
    Variable == "  Median Grade (IQR)†" ~ 5,
    Variable == "  ISS" ~ 6,
    Variable == "  GCS" ~ 7,
    TRUE ~ 99
  )) %>%
  arrange(sort_order) %>%
  select(-sort_order)
#- 2.4.10: Insert a carotid section header
carotid_section <- combined_results_raw %>% 
  filter(Vessel == "Carotid") %>%
  filter(Variable %in% c("Carotid Patients", "  MFC_present", "  BLC", "  concom_CV", "  max_carotid", "  ISS", "  GCS")) %>%
  mutate(Variable = case_when(
    Variable == "Carotid Patients" ~ "  Number of Patients",
    Variable == "  MFC_present" ~ "  Multifocal Injury",
    Variable == "  BLC" ~ "  Bilateral Injury",
    Variable == "  concom_CV" ~ "  Concomitant VBCVI",
    Variable == "  max_carotid" ~ "  Median Grade (IQR)†",
    TRUE ~ Variable
  )) %>%
  mutate(sort_order = case_when(
    Variable == "  Number of Patients" ~ 1,
    Variable == "  Multifocal Injury" ~ 2,
    Variable == "  Bilateral Injury" ~ 3,
    Variable == "  Concomitant VBCVI" ~ 4,
    Variable == "  Median Grade (IQR)†" ~ 5,
    Variable == "  ISS" ~ 6,
    Variable == "  GCS" ~ 7,
    TRUE ~ 99
  )) %>%
  arrange(sort_order) %>%
  select(-sort_order)
#- 2.4.11: Create header and blank rows
vertebral_header <- tibble(Variable = "Vertebral", N = "", Y = "", Total = "", `OR [95% CI] or p-value*` = "", Vessel = "Header")
carotid_header <- tibble(Variable = "Carotid", N = "", Y = "", Total = "", `OR [95% CI] or p-value*` = "", Vessel = "Header")
blank_row <- tibble(Variable = "", N = "", Y = "", Total = "", `OR [95% CI] or p-value*` = "", Vessel = "Blank")
#- 2.4.12: Combine in desired order
combined_results <- bind_rows(
  vertebral_header,
  vertebral_section,
  blank_row,
  carotid_header,
  carotid_section
) %>%
  select(Variable, N, Y, `OR [95% CI] or p-value*`, Total) %>%  # Reorder columns and remove Vessel
  rename(
    !!sprintf("N (n=%d, %d%%)", all_n, all_n_pct) := N,
    !!sprintf("Y (n=%d, %d%%)", all_y, all_y_pct) := Y,
    !!sprintf("Total (n=%d)", all_total) := Total
  )
#- 2.4.13: Export as excel sheet
write.xlsx(combined_results, "Outputs/Tables/ST1.xlsx")
#+ 2.5: Run a GLM on the full dataset
#- 2.5.1: Create imputed version of descriptive_data
descriptive_data_imputed_i <- raw_modeling %>%
  mutate(across(
    all_of(c(
      "stroke", "ASA", "sexM", "BLC", "BLV",
      "MFC_present", "MFV_present",
      "isolated_C", "isolated_V", "concom_CV"
    )),
    ~ factor(if_else(. == 1, "Y", "N"), levels = c("N", "Y"))
  )) %>%
  arrange(desc(stroke)) |>
  # joining in BMI and race to model in stepwise
  left_join(descriptive_data_join_2 |> select(ID, BMI, race), by = "ID") |>
  # Convert race to factor with White as reference category
  mutate(race = factor(race, levels = c("White", "Black", "Other or Unknown")))
#- 2.5.2: Impute missing BMI values using MICE (72/1197 = 6% missing)
imp_BMI <- mice(descriptive_data_imputed_i,
  m = 5,
  method = "pmm",
  seed = 2025,
  printFlag = FALSE
)
#- 2.5.3: Create complete dataset from imputed values
descriptive_data_imputed <- as_tibble(complete(imp_BMI, 1))
# Verify no missing BMI values remain
if(any(is.na(descriptive_data_imputed$BMI))) {
  stop("BMI still contains missing values after imputation")
} else {
  message("✓ BMI imputation successful: 0 missing values")
}
#- 2.5.4: Create the model formula
full_model <- glm(
  stroke ~ ASA + BLC + no_MFC + sexM + MFC_present + max_carotid + concom_CV + 
    BLV + no_MFV + MFV_present + max_vert + ISS + GCS + age + race + BMI,
  data = descriptive_data_imputed, 
  family = binomial()
)
#- 2.5.5: Run stepAIC to pare down to simplified model
adjust_model <- stepAIC(full_model, direction = "both")
#- 2.5.6: Compute ORs and 95% CIs
or_ci_table <- broom::tidy(adjust_model, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    `aOR [95% CI]` = sprintf("%.2f [%.2f–%.2f]", estimate, conf.low, conf.high),
    `p-value` = sprintf("%.0E", p.value),
    `Model Variable` = case_when(
      term == "max_carotid" ~ "Maximum Carotid Biffl Grade",
      term == "ASAY" ~ "Received AT",
      term == "max_vert" ~ "Maximum Vertebral Biffl Grade",
      TRUE ~ term
    ),
    sort_order = case_when(
      term == "max_carotid" ~ 1,
      term == "ASAY" ~ 2,
      term == "max_vert" ~ 3,
      TRUE ~ 99
    )
  ) %>%
  arrange(sort_order) %>%
  select(`Model Variable`, `aOR [95% CI]`, `p-value`)
#- 2.5.7: Export the ORs and CIs
write.xlsx(or_ci_table, "Outputs/Tables/ST2.xlsx")