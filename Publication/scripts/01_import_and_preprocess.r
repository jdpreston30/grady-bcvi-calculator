#* 1: Data import, preprocess and clean
  #+ 1.0: Skip data import below and just load RDS
    list2env(readRDS("all_objects.rds"), envir = .GlobalEnv)
  #+ 1.1: Import data
    raw_path <- "/Users/jdp2019/Desktop/merged_data_DI.xlsx" # temp test
    raw_iiii <- read_excel(raw_path, sheet = "data")
  #+ 1.2: Add max grade variable for each location
    #- 1.2.1: Define location columns
      cols_LC <- c(
        "LCCB_1", "LC1_1", "LC2_1", "LC3_1", "LC4_1", "LCO_1",
        "LCCB_2", "LC1_2", "LC2_2", "LC3_2", "LC4_2", "LCO_2")
      cols_RC <- c(
        "RCCB_1", "RC1_1", "RC2_1", "RC3_1", "RC4_1", "RCO_1",
        "RCCB_2", "RC1_2", "RC2_2", "RC3_2", "RC4_2", "RCO_2")
      cols_LV <- c(
        "LV1_1", "LV2_1", "LV3_1", "LV4_1",
        "LV1_2", "LV2_2", "LV3_2", "LV4_2",
        "LV1_3", "LV2_3", "LV3_3", "LV4_3")
      cols_RV <- c(
        "RV1_1", "RV2_1", "RV3_1", "RV4_1",
        "RV1_2", "RV2_2", "RV3_2", "RV4_2",
        "RV1_3", "RV2_3", "RV3_3", "RV4_3")
      cols_VB <- c("VB_1")
      #_Check for NAs
        cat("NA counts by location:\n")
        list(
          LC = cols_LC,
          RC = cols_RC,
          LV = cols_LV,
          RV = cols_RV,
          VB = cols_VB
        ) %>%
          lapply(\(cols) colSums(is.na(raw_iiii[, cols]))) %>%
          lapply(sum) %>%
          print()
    #- 1.2.2: Add max grade variables as well as bilateral variables
      raw_iii <- raw_iiii %>%
        mutate(
          #_ Max by territory
          Max_LC = pmax(!!!raw_iiii[, cols_LC], na.rm = TRUE),
          Max_RC = pmax(!!!raw_iiii[, cols_RC], na.rm = TRUE),
          Max_LV = pmax(!!!raw_iiii[, cols_LV], na.rm = TRUE),
          Max_RV = pmax(!!!raw_iiii[, cols_RV], na.rm = TRUE),
          Max_VB = pmax(!!!raw_iiii[, cols_VB], na.rm = TRUE)
        ) %>%
        mutate(
          #_ Max across sides
          max_vert = pmax(Max_LV, Max_RV, Max_VB),
          max_carotid = pmax(Max_RC, Max_LC)) %>%
        mutate(
          BLC = case_when(
            Max_LC == 0 & Max_RC == 0 ~ NA_real_,
            Max_LC > 0 & Max_RC > 0 ~ 1,
            TRUE ~ 0
          ),
          BLV = case_when(
            Max_LV == 0 & Max_RV == 0 ~ NA_real_,
            Max_LV > 0 & Max_RV > 0 ~ 1,
            TRUE ~ 0
          )
        )
    #- 1.2.3: Add multifocal, concom, no_segments variables, change other variables to factors
      raw <- raw_iii %>%
        mutate(
          #_ Left Carotid
          LC1_max = pmax(!!!raw_iii[, c("LC1_1", "LC1_2")], na.rm = TRUE),
          LC2_max = pmax(!!!raw_iii[, c("LC2_1", "LC2_2")], na.rm = TRUE),
          LC3_max = pmax(!!!raw_iii[, c("LC3_1", "LC3_2")], na.rm = TRUE),
          LC4_max = pmax(!!!raw_iii[, c("LC4_1", "LC4_2")], na.rm = TRUE),
          LCO_max = pmax(!!!raw_iii[, c("LCO_1", "LCO_2")], na.rm = TRUE),
          LCCB_max = pmax(!!!raw_iii[, c("LCCB_1", "LCCB_2")], na.rm = TRUE),
          #_ Right Carotid
          RC1_max = pmax(!!!raw_iii[, c("RC1_1", "RC1_2")], na.rm = TRUE),
          RC2_max = pmax(!!!raw_iii[, c("RC2_1", "RC2_2")], na.rm = TRUE),
          RC3_max = pmax(!!!raw_iii[, c("RC3_1", "RC3_2")], na.rm = TRUE),
          RC4_max = pmax(!!!raw_iii[, c("RC4_1", "RC4_2")], na.rm = TRUE),
          RCO_max = pmax(!!!raw_iii[, c("RCO_1", "RCO_2")], na.rm = TRUE),
          RCCB_max = pmax(!!!raw_iii[, c("RCCB_1", "RCCB_2")], na.rm = TRUE),
          #_ Left Vertebral
          LV1_max = pmax(!!!raw_iii[, c("LV1_1", "LV1_2", "LV1_3")], na.rm = TRUE),
          LV2_max = pmax(!!!raw_iii[, c("LV2_1", "LV2_2", "LV2_3")], na.rm = TRUE),
          LV3_max = pmax(!!!raw_iii[, c("LV3_1", "LV3_2", "LV3_3")], na.rm = TRUE),
          LV4_max = pmax(!!!raw_iii[, c("LV4_1", "LV4_2", "LV4_3")], na.rm = TRUE),
          #_ Right Vertebral
          RV1_max = pmax(!!!raw_iii[, c("RV1_1", "RV1_2", "RV1_3")], na.rm = TRUE),
          RV2_max = pmax(!!!raw_iii[, c("RV2_1", "RV2_2", "RV2_3")], na.rm = TRUE),
          RV3_max = pmax(!!!raw_iii[, c("RV3_1", "RV3_2", "RV3_3")], na.rm = TRUE),
          RV4_max = pmax(!!!raw_iii[, c("RV4_1", "RV4_2", "RV4_3")], na.rm = TRUE)
        ) %>%
        mutate(
          MFCL = as.integer(rowSums(select(., LC1_max:LCCB_max) > 0) > 1),
          MFCR = as.integer(rowSums(select(., RC1_max:RCCB_max) > 0) > 1),
          MFVL = as.integer(rowSums(select(., LV1_max:LV4_max) > 0) > 1),
          MFVR = as.integer(rowSums(select(., RV1_max:RV4_max) > 0) > 1)
        ) %>%
        mutate(
          no_MFC = MFCL + MFCR,
          no_MFV = MFVL + MFVR,
          MFC_present = as.integer(no_MFC > 0),
          MFV_present = as.integer(no_MFV > 0)
        ) %>%
        mutate(
          injury_complex = case_when(
            max_carotid > 0 & max_vert == 0 ~ "C",
            max_carotid == 0 & max_vert > 0 ~ "V",
            max_carotid > 0 & max_vert > 0 ~ "CV",
            max_carotid == 0 & max_vert == 0 ~ NA_character_
          )
        ) %>%
        mutate(
          injury_complex = factor(injury_complex, levels = c("C", "V", "CV"))
        ) %>%
        mutate(
          isolated_C = if_else(injury_complex == "C", 1, 0),
          isolated_V = if_else(injury_complex == "V", 1, 0),
          concom_CV  = if_else(injury_complex == "CV", 1, 0)
        ) %>%
        mutate(
          carotid_segments = as.integer(rowSums(select(., LC1_max:LCCB_max, RC1_max:RCCB_max) > 0, na.rm = TRUE)),
          vertebral_segments = as.integer(rowSums(select(., LV1_max:LV4_max, RV1_max:RV4_max, Max_VB) > 0, na.rm = TRUE))
        ) %>%
        mutate(
          tot_carotid_inj = rowSums(select(., all_of(c(cols_LC, cols_RC))) > 0, na.rm = TRUE),
          tot_vert_inj = rowSums(select(., all_of(c(cols_LV, cols_RV, cols_VB))) > 0, na.rm = TRUE)
        )
  #+ 1.3: Create a modeling version and impute
    #- 1.3.1: Select variables of interest; replace NAs with 0 where appropriate
      raw_modeling_i <- raw %>%
        select(ID,stroke:ISS, max_vert:BLV,no_MFC:MFV_present,isolated_C:concom_CV,MFC_present,MFV_present,carotid_segments:tot_vert_inj) %>%
        mutate(across(c(no_MFV,no_MFC,BLC,BLV), ~ replace_na(., 0))) %>%
        mutate(stroke = if_else(stroke == "Y", 1, 0)) %>%
        mutate(ASA = if_else(ASA == "Y", 1, 0)) %>%
        mutate(sexM = if_else(sex == "M", 1, 0)) %>%
        select(stroke, ASA, sexM, everything(),-sex)
    #- 1.3.2: Check NAs per column
      colSums(is.na(raw_modeling_i))
    #- 1.3.3: Impute missing values for GCS using mice
      raw_modeling <- as_tibble(complete(mice(raw_modeling_i, method = "pmm", m = 5, seed = 123), 1))
      sum(is.na(raw_modeling))
