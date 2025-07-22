#* 2: Descriptive Statistics
  #+ 2.1: Convert them all to factors with levels "N" and "Y"
    descriptive_data <- raw_modeling %>%
      mutate(across(
        all_of(c(
          "stroke", "ASA", "sexM", "BLC", "BLV",
          "MFC_present", "MFV_present",
          "isolated_C", "isolated_V", "concom_CV"
        )),
        ~ factor(if_else(. == 1, "Y", "N"), levels = c("N", "Y"))
      ))

     nested_analysis <- raw_modeling %>%
       mutate(across(
         all_of(c(
           "stroke", "ASA", "sexM", "BLC", "BLV",
           "MFC_present", "MFV_present",
           "isolated_C", "isolated_V", "concom_CV"
         )),
         ~ factor(if_else(. == 1, "Y", "N"), levels = c("N", "Y"))
       ))
  #+ 2.2: Run ternG on only the carotid injuries
    #- 2.2.1: Filter to carotid and vertebral
      carotid_only <- nested_analysis %>% filter(isolated_C == "Y" | concom_CV == "Y")
      vertebral_only <- nested_analysis %>% filter(isolated_V == "Y" | concom_CV == "Y")
    #- 2.2.2: Compute the descriptive n's separately
      stroke_counts <- function(df, label) {
        tab <- df %>%
          count(stroke) %>%
          tidyr::pivot_wider(names_from = stroke, values_from = n)

        n_N <- tab$N
        n_Y <- tab$Y
        total <- n_N + n_Y

        tibble(
          Variable = label,
          N = sprintf("%d (%.0f%%)", n_N, n_N / total * 100),
          Y = sprintf("%d (%.0f%%)", n_Y, n_Y / total * 100),
          p = NA_character_,
          test = NA_character_,
          OR = NA_character_
        )
      }
      combined_descriptives <- bind_rows(
        stroke_counts(nested_analysis, "All Patients"),
        stroke_counts(carotid_only, "Carotid Patients"),
        stroke_counts(vertebral_only, "Vertebral Patients")
      ) %>%
        mutate(Vessel = case_when(
          Variable == "Carotid Patients" ~ "Carotid",
          Variable == "Vertebral Patients" ~ "Vertebral",
          TRUE ~ "ALL"
        ))
    #- 2.2.3: Run ternG for carotid results
      carotid_results <- ternG(
        data = carotid_only,
        vars = c("BLC", "no_MFC", "MFC_present", "max_carotid", "concom_CV","carotid_segments","tot_carotid_inj","ISS","GCS"),
        group_var = "stroke",
        force_ordinal = c("max_carotid", "no_MFC"),
        output_docx = NULL,
        OR_col = TRUE
      ) %>%
        rename(Variable = 1, N = 2, Y = 3, p = 4, test = 5) %>%
        mutate(Vessel = "Carotid")
    #- 2.2.4: Run ternG for vertebral results
      vertebral_results <- ternG(
        data = vertebral_only,
        vars = c("BLV", "no_MFV", "MFV_present", "max_vert", "concom_CV","vertebral_segments","tot_vert_inj","ISS", "GCS"),
        group_var = "stroke",
        force_ordinal = c("max_vert", "no_MFV"),
        output_docx = NULL,
        OR_col = TRUE
      ) %>%
        rename(Variable = 1, N = 2, Y = 3, p = 4, test = 5) %>%
        mutate(Vessel = "Vertebral")
    #- 2.2.5: Combine the results
      combined_results <- bind_rows(
        carotid_results,
        vertebral_results,
        combined_descriptives
      ) %>%
        mutate(p_OR = if_else(is.na(OR), p, OR)) %>%
        select(-p,-OR) %>%
        arrange(desc(Vessel))      
    #- 2.2.6: Export as excel sheet
      write.xlsx(combined_results, "ST1.xlsx")
  #+ 2.3: Run a GLM on the full dataset
    #- 2.3.1: Create the model formula
      full_model <- glm(
        stroke ~ ASA + BLC + no_MFC + sexM + MFC_present + max_carotid + concom_CV + 
          BLV + no_MFV + MFV_present + max_vert + ISS + GCS + age,
        data = nested_analysis, 
        family = binomial()
      )
    #- 2.3.2: Run stepAIC to pare down to simplified model
      adjust_model <- stepAIC(full_model, direction = "both")
    #- 2.3.3: Compute ORs and 95% CIs
    or_ci_table <- broom::tidy(adjust_model, exponentiate = TRUE, conf.int = TRUE) %>%
      mutate(
        `OR [95% CI]` = sprintf("%.2f [%.2f–%.2f]", estimate, conf.low, conf.high)
      ) %>%
      select(term, `OR [95% CI]`, p.value)
    #- 2.3.4: Export the ORs and CIs
      write.xlsx(or_ci_table, "ST2.xlsx")
