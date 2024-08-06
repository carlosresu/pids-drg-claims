required_packages <- c(
  "data.table",
  "here",
  "tictoc",
  "stringr",
  "stringi",
  "lubridate",
  "docstring",
  "profvis",
  "hash",
  "future",
  "future.apply",
  "knitr",
  "htmlwidgets",
  "parallelly",
  "stringdist",
  "progress",
  "parallel"
)

# Function to install and load packages
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Install and load required packages
lapply(required_packages, install_and_load)

suppressPackageStartupMessages({
  lapply(required_packages, library, character.only = TRUE)
})

to_read <- FALSE # TODO: Deprecated, used to be whether to forcibly read the whole file again instead of using the split parts created even if available
to_split <- TRUE # TODO: Deprecated, only used when to_sample is TRUE # Whether to split into split_parts parts (i.e. to fit in 32gb RAM).

tic("Time spent (total)               ") # Start total execution timer

na_values <- c("NONE", "None", "-", "--", "---", "N/A", "n/a", "nan", "NAN")
na_like_strings <- c(
  "", " ", "  ", " ", "-", "none", "None", "NONE", "NA", "n/a",
  "N/A", "NaN", "'", "\t", "\n", "\r", "\f", "\v", "\u00A0",
  "\u2000", "\u2001", "\u2002", "\u2003", "\u2004", "\u2005",
  "\u2006", "\u2007", "\u2008", "\u2009", "\u200A", "\u2028",
  "\u2029", "\u202F", "\u205F", "\u3000"
)

all_na_values <- unique(c(na_values, na_like_strings))

integer_cols <- c("OUT_PATIENT", "EMERGENCY")

factor_cols <- c(
  "PATIENT_TYPE", "ROOM_TYPE", "DEP_REL", "PATSEX", "MEMCAT_PARENT_DESC",
  "MEMCAT_CHILD_DESC",
  # "MEMCAT_SUBCHILD_DESC",
  "DISPOSITION", "CLAIMS_STATUS"
)
numeric_cols <- c(
  "PATAGE", "PAT_BWT_KG", "CLAIMS_PAID_AMT",
  "ACR_AMOUNT_ACTUAL"
)

character_cols <- c(
  "PSEUDO_CLAIMSERIES", "PSEUDO_MEM_PIN", "HCI_PMCC_NO", "HCP_NO_LIST",
  "PRIMARY_ILLNESS", "SECONDARY_ILLNESS", paste0("ICDCODE", c(1:12)),
  paste0("RVSCODE", 1:20), "DATE_ADM", "TIME_ADM",
  "DATE_DIS", "TIME_DIS", "DATE_REC", "DATE_REF", "CHKDT",
  "PAT_BDAY", "EXTRACTION_DATE"
)

# Define column classes
col_classes <- c(
  rep("character", length(character_cols)),
  rep("integer", length(integer_cols)),
  rep("factor", length(factor_cols)),
  rep("numeric", length(numeric_cols))
)

names(col_classes) <- c(
  character_cols, integer_cols,
  factor_cols, numeric_cols
)

covid_rvs <- c(
  "C19T1", "C19T2", "C19T3", "C19X1", "C19X2", "C19X3", "C19FRP",
  "C19IP1", "C19IP2", "C19IP3", "C19IP4", "C19PP1", "C19PP2",
  "C19PP3", "C19PP4", "MP01", "IMP02", "C19CI", "C19H1", "C19VIH",
  "C19VID"
)

old_colnames <- c(
  "SRC_YR", "PSEUDO_CLAIMSERIES", "PSEUDO_MEM_PIN", "DATE_ADM", "TIME_ADM",
  "DATE_DIS", "TIME_DIS", "DATE_REC", "DATE_REF", "CHKDT", "EXTRACTION_DATE",
  "HCI_PMCC_NO", "HCP_NO_LIST", "PATIENT_TYPE", "DEP_REL", "PATSEX", "PATAGE",
  "PAT_BDAY", "PAT_BWT_KG", "MEMCAT_PARENT_DESC", "MEMCAT_CHILD_DESC",
  # "MEMCAT_SUBCHILD_DESC",
  "OUT_PATIENT", "EMERGENCY", "ROOM_TYPE",
  "DISPOSITION", "PRIMARY_ILLNESS", "SECONDARY_ILLNESS",
  paste0("ICDCODE", 1:12), paste0("RVSCODE", 1:20),
  "CLAIMS_STATUS", "ACR_AMOUNT_ACTUAL", "CLAIMS_PAID_AMT"
)

new_colnames <- c(
  "id_year", "id_series", "id_pin", "date_adm", "time_adm",
  "date_dis", "time_dis", "date_rec", "date_ref", "date_check", "date_ext",
  "id_hci", "id_hcp", "pat_type", "pat_rel", "pat_sex", "pat_age",
  "pat_bdate", "pat_bwt", "pat_memcat_parent", "pat_memcat_child",
  # "pat_memcat_subchild",
  "clin_outpatient", "clin_emergency", "clin_acc",
  "clin_discharge", "clin_c1", "clin_c2", paste0("clin_icd", 1:12),
  paste0("clin_rvs", 1:20), "claim_status", "claim_charge", "claim_payout"
)

# Paths to various directories for intermediate files,
# cache, auxiliary files, etc.
# Create the directory if it does not exist
if (!dir.exists(here(intermediate_path))) {
  dir.create(here(intermediate_path), recursive = TRUE)
  cat("Directory created:", intermediate_path, "\n")
} else {
  cat("Directory already exists:", intermediate_path, "\n")
}

# Create the directory if it does not exist
if (!dir.exists(here(cache_path))) {
  dir.create(here(cache_path), recursive = TRUE)
  cat("Directory created:", cache_path, "\n")
} else {
  cat("Directory already exists:", cache_path, "\n")
}

# Create the directory if it does not exist
if (!dir.exists(here(aux_path))) {
  dir.create(here(aux_path), recursive = TRUE)
  cat("Directory created:", aux_path, "\n")
} else {
  cat("Directory already exists:", aux_path, "\n")
}

# Create the directory if it does not exist
if (!dir.exists(here(excel_path))) {
  dir.create(here(excel_path), recursive = TRUE)
  cat("Directory created:", excel_path, "\n")
} else {
  cat("Directory already exists:", excel_path, "\n")
}

# Create the directory if it does not exist
if (!dir.exists(here(cleaned_claims_path))) {
  dir.create(here(cleaned_claims_path), recursive = TRUE)
  cat("Directory created:", cleaned_claims_path, "\n")
} else {
  cat("Directory already exists:", cleaned_claims_path, "\n")
}

# Create the directory if it does not exist
if (!dir.exists(here(grouper_output_path))) {
  dir.create(here(grouper_output_path), recursive = TRUE)
  cat("Directory created:", grouper_output_path, "\n")
} else {
  cat("Directory already exists:", grouper_output_path, "\n")
}

# Create the directory if it does not exist
if (!dir.exists(here(chunks_path))) {
  dir.create(here(chunks_path), recursive = TRUE)
  cat("Directory created:", chunks_path, "\n")
} else {
  cat("Directory already exists:", chunks_path, "\n")
}

# Create the directory if it does not exist
if (!dir.exists(here(raw_claims_parts_path))) {
  dir.create(here(raw_claims_parts_path), recursive = TRUE)
  cat("Directory created:", raw_claims_parts_path, "\n")
} else {
  cat("Directory already exists:", raw_claims_parts_path, "\n")
}

# Create the directory if it does not exist
if (!dir.exists(here(raw_claims_samples_path))) {
  dir.create(here(raw_claims_samples_path), recursive = TRUE)
  cat("Directory created:", raw_claims_samples_path, "\n")
} else {
  cat("Directory already exists:", raw_claims_samples_path, "\n")
}

# Create the directory if it does not exist
if (!dir.exists(here(raw_claims_path))) {
  dir.create(here(raw_claims_path), recursive = TRUE)
  cat("Directory created:", raw_claims_path, "\n")
} else {
  cat("Directory already exists:", raw_claims_path, "\n")
}

# Create the directory if it does not exist
if (!dir.exists(here("data-cleaning/data/profvis"))) {
  dir.create(here("data-cleaning/data/profvis"), recursive = TRUE)
  cat("Directory created:", "data-cleaning/data/profvis", "\n")
} else {
  cat("Directory already exists:", "data-cleaning/data/profvis", "\n")
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
    paste0("claims_extract_CLAIMS ", year_to_load)
  } else {
    paste0(
      "claims_extract_CLAIMS ", year_to_load,
      "_part_", sprintf("%02d", part), "_of_", split_parts
    )
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
      sprintf("%02d", part), "_of_", split_parts
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
      "_part_", sprintf("%02d", part), "_of_", split_parts
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
      "part_", sprintf("%02d", part), "_of_", split_parts
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
      "part_", sprintf("%02d", part), "_of_", split_parts
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
      "part_", sprintf("%02d", part), "_of_", split_parts
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
      "Res_", sprintf("%02d", part), "_of_", split_parts
    ))
  }
  if (fileext) {
    filename <- paste0(filename, ".TXT")
  }
  return(here(grouper_output_path, filename))
}
