# Paths to various directories for intermediate files,
# cache, auxiliary files, etc.
intermediate_path <- "data-claims/intermediate"
cache_path <- "data-cleaning/cache"
aux_path <- "data-aux-files"
excel_path <- "data-excel"
cleaned_claims_path <- "data-claims/cleaned"
grouper_output_path <- "data-grouper-output"
chunks_path <- "data-claims/chunked"
raw_claims_parts_path <- "data-claims/raw/parts"
raw_claims_samples_path <- "data-claims/raw/samples"
raw_claims_path <- "data-claims"
profvis_path <- "profvis/profvis.html"

full_claims_file <- function(part = NULL, fileext = TRUE) {
  #' @title Generate the file path for the full claims file
  #'
  #' @description This function generates the file path for the
  #' full claims file, based on the year and part.
  #'
  #' @param part Integer. The part number of the file.
  #' Default is NULL.
  #' @param fileext Logical. Whether to include the file extension.
  #' Default is TRUE.
  #'
  #' @return Character. The generated file path.
  filename <- if (is.null(part)) {
    paste0("claims_extract_CLAIMS ", year_to_load)
  } else {
    paste0("claims_extract_CLAIMS ", year_to_load, "_part_", part, "_of_", split_parts)
  }
  if (fileext) filename <- paste0(filename, ".csv")
  if (is.null(part)) {
    return(here(raw_claims_path, filename))
  } else {
    return(here(raw_claims_parts_path, filename))
  }
}

total_rows_file <- function(part = NULL, fileext = TRUE) {
  #' @title Generate the file path for total rows file
  #'
  #' @description This function generates the file path for storing/retrieving
  #' the total number of rows in a claims file, based on the year and part.
  #'
  #' @param part Integer. The part number of the file.
  #' Default is NULL.
  #' @param fileext Logical. Whether to include the file extension.
  #' Default is TRUE.
  #'
  #' @return Character. The generated file path.
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
  return(here(cache_path, filename))
}

# Load cached total rows file if available, saves ~10 seconds of runtime
if (file.exists(total_rows_file())) {
  total_rows <- readRDS(total_rows_file())
  cat(paste("Total Rows via cached object:", total_rows))
} else {
  total_rows <- fread(full_claims_file(), select = 1L, header = TRUE)[, .N]
  saveRDS(total_rows, file = total_rows_file())
  cat(paste("Total Rows via fread:", total_rows))
}

# Compute sample size when splitting and when not,
# only relevant when sampling
if (to_split) {
  sample_size <- ceiling(total_rows / split_parts / sample_size_divisor)
} else {
  sample_size <- ceiling(total_rows / sample_size_divisor)
}

suffix <- paste0(
  ifelse(to_sample, paste0("_sampled_", sample_size, "_"), "_full_")
)

sampled_claims_file <- function(part = NULL, fileext = TRUE) {
  #' @title Generate the file path for the sampled claims file
  #'
  #' @description This function generates the file path for the sampled
  #' claims file, based on the year, sample size, and part.
  #'
  #' @param part Integer. The part number of the file.
  #' Default is NULL.
  #' @param fileext Logical. Whether to include the file extension.
  #' Default is TRUE.
  #'
  #' @return Character. The generated file path.
  filename <- if (is.null(part)) {
    paste0("sampled_claims_", year_to_load, "_", sample_size)
  } else {
    paste0(
      "sampled_claims_", year_to_load, "_", sample_size,
      "_part_", part, "_of_", split_parts
    )
  }
  if (fileext) filename <- paste0(filename, ".csv")
  return(here(raw_claims_samples_path, filename))
}

intermediate_file <- function(part = NULL, fileext = TRUE) {
  #' @title Generate the file path for the intermediate claims file
  #'
  #' @description This function generates the file path for the
  #' intermediate claims file, based on the year, suffix, and part.
  #'
  #' @param part Integer. The part number of the file.
  #' Default is NULL.
  #' @param fileext Logical. Whether to include the file extension.
  #' Default is TRUE.
  #'
  #' @return Character. The generated file path.
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
  return(here(intermediate_path, filename))
}

cleaned_claims_file <- function(part = NULL, fileext = TRUE) {
  #' @title Generate the file path for the cleaned claims file
  #'
  #' @description This function generates the file path for the cleaned
  #' claims file, based on the year, suffix, and part.
  #'
  #' @param part Integer. The part number of the file.
  #' Default is NULL.
  #' @param fileext Logical. Whether to include the file extension.
  #' Default is TRUE.
  #'
  #' @return Character. The generated file path.
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
  return(here(cleaned_claims_path, filename))
}

output_txt_file <- function(part = NULL, fileext = TRUE) {
  #' @title Generate the file path for the output text file for DRG grouping
  #'
  #' @description This function generates the file path for the output text file
  #' for DRG grouping, based on the year, suffix, and part.
  #'
  #' @param part Integer. The part number of the file.
  #' Default is NULL.
  #' @param fileext Logical. Whether to include the file extension.
  #' Default is TRUE.
  #'
  #' @return Character. The generated file path.
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
  return(here(grouper_output_path, filename))
}

grouper_result_file <- function(part = NULL, fileext = TRUE) {
  #' @title Generate the file path for the grouper result file
  #'
  #' @description This function generates the file path for the
  #' grouper result file, based on the year, suffix, and part.
  #'
  #' @param part Integer. The part number of the file.
  #' Default is NULL.
  #' @param fileext Logical. Whether to include the file extension.
  #' Default is TRUE.
  #'
  #' @return Character. The generated file path.
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
  return(here(grouper_output_path, filename))
}
