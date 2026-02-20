#! Data merging script NOT part of pipeline, used to merge old and new datasets for descriptive analyses and model development. Not intended for regular use, but kept for reproducibility and reference.
#* Import data
#+ Set paths
clean_old_data <- "/Users/jdp2019/Library/CloudStorage/OneDrive-Emory/Research/Manuscripts and Projects/Grady/Risk Calculator/Raw Data/Clean_old_data.xlsx"
clean_new_data <- "/Users/jdp2019/Library/CloudStorage/OneDrive-Emory/Research/Manuscripts and Projects/Grady/Risk Calculator/Raw Data/Clean_new_data.xlsx"
#+ Old Data
major_data_old_i <- read_excel(clean_old_data, sheet = "major_data")
metadata_old <- read_excel(clean_old_data, sheet = "metadata") %>%
  select(ID, sex, age, GCS, ISS,death_location) %>%
  mutate(DC_status = if_else(death_location == "Alive", "Alive", "Dead")) %>%
  select(-death_location)
stroke_metadata_old <- read_excel(clean_old_data, sheet = "stroke_metadata") %>%
  select(ID, hrs_to_stroke)
#+ New Data
major_data_new <- read_excel(clean_new_data, sheet = "major_data")
metadata_new <- read_excel(clean_new_data, sheet = "metadata") %>%
  select(ID, sex, age, GCS, ISS,DC_status)
stroke_metadata_new <- read_excel(clean_new_data, sheet = "stroke_metadata") %>%
  select(ID, hrs_to_stroke)
#* Preprocess and merge the major data
#+ Merge the CC and CB injuries by taking max of each
major_data_old <- major_data_old_i %>%
  mutate(
  LCCB_1 = pmax(LCC_1, LCB_1, na.rm = TRUE),
  LCCB_2 = pmax(LCC_2, LCB_2, na.rm = TRUE),
  RCCB_1 = pmax(RCC_1, RCB_1, na.rm = TRUE),
  RCCB_2 = pmax(RCC_2, RCB_2, na.rm = TRUE)
) %>%
select(-LCC_1, -LCB_1, -LCC_2, -LCB_2, -RCC_1, -RCB_1, -RCC_2, -RCB_2)
major_data_old <- major_data_old[, colnames(major_data_new)]
#+ Ensure the column names match between old and new
identical(colnames(major_data_new), colnames(major_data_old))
#+ Merge
combined_major_data <- bind_rows(major_data_old, major_data_new)
#+ Check for any NAs
sum(is.na(combined_major_data))
#* Join the metadata and stroke metadata, the merge
#+ Join the metadata with stroke metadata
meta_w_stroke_old <- metadata_old %>%
  left_join(stroke_metadata_old, by = "ID")
meta_w_stroke_new <- metadata_new %>%
  left_join(stroke_metadata_new, by = "ID")
#+ Join the stroke metadata
combined_metadata <- bind_rows(meta_w_stroke_old, meta_w_stroke_new)
#* Now combine everything, save
combined_data <- combined_major_data %>%
  left_join(combined_metadata, by = "ID") %>%
  select(ID:ASA,sex:hrs_to_stroke,LCCB_1:VB_3)
write_xlsx(
  combined_data,
  "/Users/jdp2019/Library/CloudStorage/OneDrive-Emory/Research/Manuscripts and Projects/Grady/Risk Calculator/Raw Data/merged_data_DI.xlsx"
)