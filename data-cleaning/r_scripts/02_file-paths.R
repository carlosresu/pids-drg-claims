# Paths to various directories for intermediate files,
# cache, auxiliary files, etc.
intermediate_path <- "data-cleaning/data-claims/intermediate"
# Create the directory if it does not exist
if (!dir.exists(here(intermediate_path))) {
  dir.create(here(intermediate_path), recursive = TRUE)
  cat("Directory created:", intermediate_path, "\n")
} else {
  cat("Directory already exists:", intermediate_path, "\n")
}

cache_path <- "data-cleaning/cache"
aux_path <- "data-cleaning/data-aux-files"
# Create the directory if it does not exist
if (!dir.exists(here(aux_path))) {
  dir.create(here(aux_path), recursive = TRUE)
  cat("Directory created:", aux_path, "\n")
} else {
  cat("Directory already exists:", aux_path, "\n")
}

excel_path <- "data-cleaning/data-excel"
# Create the directory if it does not exist
if (!dir.exists(here(excel_path))) {
  dir.create(here(excel_path), recursive = TRUE)
  cat("Directory created:", excel_path, "\n")
} else {
  cat("Directory already exists:", excel_path, "\n")
}

cleaned_claims_path <- "data-cleaning/data-claims/cleaned"
# Create the directory if it does not exist
if (!dir.exists(here(cleaned_claims_path))) {
  dir.create(here(cleaned_claims_path), recursive = TRUE)
  cat("Directory created:", cleaned_claims_path, "\n")
} else {
  cat("Directory already exists:", cleaned_claims_path, "\n")
}

grouper_output_path <- "data-cleaning/data-grouper-output"
# Create the directory if it does not exist
if (!dir.exists(here(grouper_output_path))) {
  dir.create(here(grouper_output_path), recursive = TRUE)
  cat("Directory created:", grouper_output_path, "\n")
} else {
  cat("Directory already exists:", grouper_output_path, "\n")
}

chunks_path <- "data-cleaning/data-claims/chunked"
# Create the directory if it does not exist
if (!dir.exists(here(chunks_path))) {
  dir.create(here(chunks_path), recursive = TRUE)
  cat("Directory created:", chunks_path, "\n")
} else {
  cat("Directory already exists:", chunks_path, "\n")
}

raw_claims_parts_path <- "data-cleaning/data-claims/raw/parts"
# Create the directory if it does not exist
if (!dir.exists(here(raw_claims_parts_path))) {
  dir.create(here(raw_claims_parts_path), recursive = TRUE)
  cat("Directory created:", raw_claims_parts_path, "\n")
} else {
  cat("Directory already exists:", raw_claims_parts_path, "\n")
}

raw_claims_samples_path <- "data-cleaning/data-claims/raw/samples"
# Create the directory if it does not exist
if (!dir.exists(here(raw_claims_samples_path))) {
  dir.create(here(raw_claims_samples_path), recursive = TRUE)
  cat("Directory created:", raw_claims_samples_path, "\n")
} else {
  cat("Directory already exists:", raw_claims_samples_path, "\n")
}

raw_claims_path <- "data-cleaning/data-claims/raw"
# Create the directory if it does not exist
if (!dir.exists(here(raw_claims_path))) {
  dir.create(here(raw_claims_path), recursive = TRUE)
  cat("Directory created:", raw_claims_path, "\n")
} else {
  cat("Directory already exists:", raw_claims_path, "\n")
}

profvis_path <- "data-cleaning/profvis/profvis.html"
# Create the directory if it does not exist
if (!dir.exists(here("data-cleaning/profvis"))) {
  dir.create(here("data-cleaning/profvis"), recursive = TRUE)
  cat("Directory created:", "data-cleaning/profvis", "\n")
} else {
  cat("Directory already exists:", "data-cleaning/profvis", "\n")
}

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
    if (!is.null(gcp_proj) && gcp_proj == "drg-pipeline") {
      paste0("claims_extract_CLAIMS ", year_to_load)
    } else if (!is.null(gcp_proj) && gcp_proj == "gphdrg") {
      paste0("claims_extract_CLAIMS_", year_to_load)
    } else {
      paste0("claims_extract_CLAIMS_", year_to_load)
    }
  } else {
    if (!is.null(gcp_proj) && gcp_proj == "drg-pipeline") {
      paste0("claims_extract_CLAIMS ", year_to_load, "_part_", part, "_of_", split_parts)
    } else if (!is.null(gcp_proj) && gcp_proj == "gphdrg") {
      paste0("claims_extract_CLAIMS_", year_to_load, "_part_", part, "_of_", split_parts)
    } else {
      paste0("claims_extract_CLAIMS_", year_to_load, "_part_", part, "_of_", split_parts)
    }
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
