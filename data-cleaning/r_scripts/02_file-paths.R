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

total_rows_file <- function(part = NULL, fileext = TRUE) {
  filename <- if (is.null(part)) {
    paste0("total_rows_", year_to_load)
  } else {
    paste0(
      "total_rows_", year_to_load, "_part_",
      part, "_of_", split_parts
    )
  }
  if (fileext) {
    filename <- paste0(filename, ".rds")
  }
  return(here(path_to_cache, filename))
}

# Load cached total rows file if available, saves ~10 seconds of runtime
if (file.exists(total_rows_file())) {
  total_rows <- readRDS(total_rows_file())
  print(paste("Total Rows via cached object:", total_rows))
} else {
  total_rows <- fread(full_claims_file(), select = 1L, header = TRUE)[, .N]
  saveRDS(total_rows, file = total_rows_file())
  print(paste("Total Rows via fread:", total_rows))
}

# Compute sample size when splitting and when not,
# only relevant when sampling
if (to_split) {
  sample_size <- ceiling(total_rows / split_parts / sample_size_divisor)
} else {
  sample_size <- ceiling(total_rows / sample_size_divisor)
}

suffix <- paste0(ifelse(to_sample, paste0("_sampled_", sample_size, "_"), "_full_"))

full_claims_file <- function(part = NULL, fileext = TRUE) {
  filename <- if (is.null(part)) {
    paste0("full_claims_", year_to_load)
  } else {
    paste0("full_claims_", year_to_load, "_part_", part, "_of_", split_parts)
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

sampled_claims_file <- function(part = NULL, fileext = TRUE) {
  filename <- if (is.null(part)) {
    paste0(
      "sampled_claims_", year_to_load, "_",
      sample_size
    )
  } else {
    paste0(
      "sampled_claims_", year_to_load, "_",
      sample_size, "_part_", part, "_of_", split_parts
    )
  }
  if (fileext) {
    filename <- paste0(filename, ".csv")
  }
  return(here(path_to_raw_claims_samples, filename))
}

intermediate_file <- function(part = NULL, fileext = TRUE) {
  filename <- if (is.null(part)) {
    paste0("intermediate_claims_", year_to_load, suffix)
  } else {
    paste0(
      "intermediate_claims_", year_to_load, suffix,
      "part_", part, "_of_", split_parts
    )
  }
  if (fileext) {
    filename <- paste0(filename, ".csv")
  }
  return(here(path_to_intermediate, filename))
}

cleaned_claims_file <- function(part = NULL, fileext = TRUE) {
  filename <- if (is.null(part)) {
    paste0("cleaned_claims_", year_to_load, suffix)
  } else {
    paste0(
      "cleaned_claims_", year_to_load, suffix,
      "part_", part, "_of_", split_parts
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
      "part_", part, "_of_", split_parts
    )
  }
  if (fileext) {
    filename <- paste0(filename, ".txt")
  }
  return(here(path_to_grouper_output, filename))
}

grouper_result_file <- function(part = NULL, fileext = TRUE) {
  filename <- if (is.null(part)) {
    toupper(paste0(
      "DRG_Grouped", "_", year_to_load, suffix,
      "Res"
    ))
  } else {
    toupper(paste0(
      "DRG_Grouped", "_", year_to_load, suffix,
      "Res_", part, "_of_", split_parts
    ))
  }
  if (fileext) {
    filename <- paste0(filename, ".TXT")
  }
  return(here(path_to_grouper_output, filename))
}
