# source(here("data-cleaning", "r_scripts", "libraries.R"))

path_to_intermediate <- "git-ignored-files/intermediate-claims"
path_to_cache <- "data-cleaning/cache"
path_to_aux <- "git-ignored-files/aux-files"
path_to_excel <- "git-ignored-files/Excel"
path_to_cleaned_claims <- "git-ignored-files/cleaned-claims"
path_to_grouper_output <- "git-ignored-files/grouper-output"
path_to_chunks <- "git-ignored-files/chunked-samples"
path_to_raw_claims_parts <- "git-ignored-files/raw-claims/parts"
path_to_raw_claims_samples <- "git-ignored-files/raw-claims/samples"
path_to_raw_claims <- "git-ignored-files/raw-claims"

# Here() let's you find files in your project directory
suffix <- paste0(ifelse(to_sample, "_sampled_", "_full_"), version)

sampled_claims_file <- function(part = NULL, fileext = TRUE) {
  filename <- if (is.null(part)) {
    paste0("sampled_claims_extract_CLAIMS_", year_to_load, suffix, "_", sample_size)
  } else {
    paste0("sampled_claims_extract_CLAIMS_", year_to_load, suffix, "_", sample_size, "_part_", part, "_of_", split_chunks)
  }
  if (fileext) {
    filename <- paste0(filename, ".csv")
  }
  return(here(path_to_raw_claims_samples, filename))
}

full_claims_file <- function(part = NULL, fileext = TRUE) {
  filename <- if (is.null(part)) {
    paste0("claims_extract_CLAIMS_", year_to_load)
  } else {
    paste0("claims_extract_CLAIMS_", year_to_load, "_part_", part, "_of_", split_chunks)
  }
  if (fileext) {
    filename <- paste0(filename, ".csv")
  }
  if (is.null(part)) {
    return(here(path_to_raw_claims, filename))
  } else {
    return(here(path_to_raw_claims_parts, filename))
  }
}

intermediate_file <- function(part = NULL, fileext = TRUE) {
  filename <- if (is.null(part)) {
    paste0("intermediate_claims_", year_to_load, "_processed", suffix)
  } else {
    paste0(
      "intermediate_claims_", year_to_load,
      "_processed", suffix, "_part_", part, "_of_", split_chunks
    )
  }
  if (fileext) {
    filename <- paste0(filename, ".csv")
  }
  return(here(path_to_intermediate, filename))
}

cleaned_claims_file <- function(part = NULL, fileext = TRUE) {
  filename <- if (is.null(part)) {
    paste0("cleaned_claims_extract_CLAIMS_", year_to_load, suffix)
  } else {
    paste0(
      "cleaned_claims_extract_CLAIMS_",
      year_to_load, suffix, "_part_", part, "_of_", split_chunks
    )
  }
  if (fileext) {
    filename <- paste0(filename, ".csv")
  }
  return(here(path_to_cleaned_claims, filename))
}

output_txt_file <- function(part = NULL, fileext = TRUE) {
  filename <- if (is.null(part)) {
    paste0("DRG_Grouped", "_", year_to_load, suffix)
  } else {
    paste0(
      "DRG_Grouped", "_", year_to_load, suffix,
      "_part_", part, "_of_", split_chunks
    )
  }
  if (fileext) {
    filename <- paste0(filename, ".txt")
  }
  return(here(path_to_grouper_output, filename))
}

grouper_result_file <- function(part = NULL, fileext = TRUE) {
  filename <- if (is.null(part)) {
    toupper(paste0("DRG_Grouped", "_", year_to_load, suffix, "Res"))
  } else {
    toupper(paste0(
      "DRG_Grouped", "_", year_to_load,
      suffix, "Res_", part, "_of_", split_chunks
    ))
  }
  if (fileext) {
    filename <- paste0(filename, ".TXT")
  }
  return(here(path_to_grouper_output, filename))
}

total_rows_file <- function(part = NULL, fileext = TRUE) {
  filename <- if (is.null(part)) {
    paste0("total_rows_", year_to_load)
  } else {
    paste0(
      "total_rows_", year_to_load, "_part_",
      part, "_of_", split_chunks
    )
  }
  if (fileext) {
    filename <- paste0(filename, ".rds")
  }
  return(here(path_to_cache, filename))
}
