shuffle_rows_by_id <- function(df, id_col = "ID", seed = 2025) {
  # Sort by ID to ensure a consistent baseline
  df_sorted <- df %>% arrange(!!sym(id_col))
  # Create reproducible random permutation with fixed seed
  set.seed(seed)
  shuffled_indices <- sample(seq_len(nrow(df_sorted)))
  # Return shuffled dataset
  df_sorted[shuffled_indices, , drop = FALSE]
}