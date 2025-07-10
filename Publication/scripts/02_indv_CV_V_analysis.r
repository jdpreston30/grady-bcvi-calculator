#* 2: Individual analysis of CV+V and CV+C injuries
  #+ 2.1: Convert them all to factors with levels "N" and "Y"
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
    #- 2.2.2: Run ternG
      carotid_results <- ternG(
        data = carotid_only,
        vars = c("BLC", "no_MFC", "MFC_present", "max_carotid", "concom_CV","carotid_segments","tot_carotid_inj"),
        group_var = "stroke",
        force_ordinal = c("max_carotid", "no_MFC"),
        output_docx = "carotid_only.docx"
      )
      vertebral_results <- ternG(
        data = vertebral_only,
        vars = c("BLV", "no_MFV", "MFV_present", "max_vert", "concom_CV","vertebral_segments","tot_vert_inj"),
        group_var = "stroke",
        force_ordinal = c("max_vert", "no_MFV"),
        output_docx = "vertebral_only.docx"
      )