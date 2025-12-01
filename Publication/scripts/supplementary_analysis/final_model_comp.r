#* 7: Generate Final Model Comparison Table
  #+ 7.1: Filter down to chosen model.Selector(
  chosen_model_sum_i <- all_model_summary %>%
    filter(Chosen == "Y")
  #+ 7.2: Add on PRAUC to table
  chosen_model_sum <- chosen_model_sum_i %>%
    left_join(pr_auc_summary, by = "Model")
  #! There are obvious issues with using the PRAUC for stuff that was downsampled and weighted, so will not use