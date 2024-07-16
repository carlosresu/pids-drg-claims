path_to_raw_claims <- "git-ignored-files/raw-claims"
path_to_intermediate <- "git-ignored-files/intermediate-claims"
path_to_cache <- "data-cleaning/cache"
path_to_aux <- "git-ignored-files/aux-files"
path_to_excel <- "git-ignored-files/Excel"
path_to_cleaned_claims <- "git-ignored-files/cleaned-claims"
path_to_grouper_output <- "git-ignored-files/grouper-output"
path_to_chunks <- "git-ignored-files/chunked-samples"

# Here() let's you find files in your project directory
suffix <- paste0(ifelse(to_sample, "_sampled_", "_full_"), version)
sampled_claims <- here(
  path_to_raw_claims,
  paste0("sampled_claims_extract_CLAIMS_", year_to_load, 
         suffix, paste0("_", sample_size), ".csv"))
full_claims <- here(
  path_to_raw_claims,
  paste0("claims_extract_CLAIMS_", year_to_load, ".csv"))
intermediate_file <- here(
  path_to_intermediate,
  paste0("intermediate_claims_", year_to_load, "_processed", suffix, ".csv"))
cleaned_claims_file <- here(
  path_to_cleaned_claims,
  paste0("cleaned_claims_extract_CLAIMS_", year_to_load, suffix, ".csv"))
output_txt_file <- here(
  path_to_grouper_output,
  paste0("DRG_Grouped", "_", year_to_load, suffix, ".txt"))
grouper_result_file <- here(
  path_to_grouper_output,
  toupper(paste0("DRG_Grouped", "_", year_to_load, suffix, "Res.TXT")))
total_rows_file <- here(
  path_to_cache,
  paste0("total_rows_", year_to_load, ".rds")
)
