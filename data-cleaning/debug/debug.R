## Required packages
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
  "parallel",
  "reticulate",
  "bigrquery",
  "jsonlite",
  "googleCloudStorageR"
)
## Install required packages
lapply(required_packages, function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
})

# Load required packages
suppressPackageStartupMessages({
  lapply(required_packages, library, character.only = TRUE)
})

to_read <- FALSE # TODO: Deprecated, used to be whether to forcibly read the whole file again instead of using the split parts created even if available
to_split <- TRUE # TODO: Deprecated, only used when to_sample is TRUE # Whether to split into split_parts parts (i.e. to fit in 32gb RAM).

## Start total execution timer
tic("Time spent (total)               ")

## NA-like strings
na_values <- c("NONE", "None", "-", "--", "---", "N/A", "n/a", "nan", "NAN")
na_like_strings <- c(
  "", " ", "  ", " ", "-", "none", "None", "NONE", "NA", "n/a",
  "N/A", "NaN", "'", "\t", "\n", "\r", "\f", "\v", "\u00A0",
  "\u2000", "\u2001", "\u2002", "\u2003", "\u2004", "\u2005",
  "\u2006", "\u2007", "\u2008", "\u2009", "\u200A", "\u2028",
  "\u2029", "\u202F", "\u205F", "\u3000"
)

# Mapping of column names (including clin_icd and clin_rvs)
column_mappings <- list(
  # Admission and discharge information
  "ADMISSION_YEAR" = "id_year",
  "SRC_YR" = "id_year",
  "ADMISSION_DATE" = "date_adm",
  "DATE_ADM" = "date_adm",
  "ADMISSION_TIME" = "time_adm",
  "TIME_ADM" = "time_adm",
  "DISCHARGE_DATE" = "date_dis",
  "DATE_DIS" = "date_dis",
  "DISCHARGE_TIME" = "time_dis",
  "TIME_DIS" = "time_dis",

  # Claim and patient identifiers
  "CLAIM_SERIES_ID" = "id_series",
  "PSEUDO_CLAIMSERIES" = "id_series",
  "PIN" = "id_pin",
  "PSEUDO_MEM_PIN" = "id_pin",

  # Date information
  "RECEIVE_DATE" = "date_rec",
  "DATE_REC" = "date_rec",
  "REFILE_DATE" = "date_ref",
  "DATE_REF" = "date_ref",
  "CHECK_DATE" = "date_check",
  "CHKDT" = "date_check",
  "EXTRACTION_DATE" = "date_ext",

  # Health care provider and institution
  "HCI_PMCC_NO" = "id_hci",
  "HCP_NO_LIST" = "id_hcp",

  # Patient information
  "PATIENT_TYPE" = "pat_type",
  "PATIENT_RELATIONSHIP" = "pat_rel",
  "DEP_REL" = "pat_rel",
  "PATIENT_SEX" = "pat_sex",
  "PATSEX" = "pat_sex",
  "PATIENT_AGE" = "pat_age",
  "PATAGE" = "pat_age",
  "PAT_BDAY" = "pat_bdate",
  "PAT_BWT_KG" = "pat_bwt",
  "MEMCAT_PARENT_DESC" = "pat_memcat_parent",
  "MEMCAT_CHILD_DESC" = "pat_memcat_child",

  # Clinical information
  "IS_ADMISSION_OPD" = "clin_outpatient",
  "IS_EMERGENCY_CASE" = "clin_emergency",
  "OUT_PATIENT" = "clin_outpatient",
  "EMERGENCY" = "clin_emergency",
  "ROOM_TYPE" = "clin_acc",
  "PATIENT_DISPOSITION" = "clin_discharge",
  "DISPOSITION" = "clin_discharge",
  "PRIMARY_ILLNESS" = "clin_c1",
  "SECONDARY_ILLNESS" = "clin_c2",
  "ICDCODES_ITEM7" = "clin_icd1", # We'll dynamically handle clin_icd and clin_rvs
  "RVSCODES_ITEM7" = "clin_rvs1",

  # Claim status and amounts
  "CLAIM_STATUS" = "claim_status",
  "CLAIMS_STATUS" = "claim_status",
  "CLAIM_PAID_AMOUNT" = "claim_payout",
  "CLAIMS_PAID_AMT" = "claim_payout",
  "CLAIM_AMOUNT_ACTUAL" = "claim_charge",
  "ACR_AMOUNT_ACTUAL" = "claim_charge"
)

# Add dynamically generated clin_icd1 through clin_icd12 and clin_rvs1 through clin_rvs20
for (i in 1:12) {
  column_mappings[[paste0("ICDCODE", i)]] <- paste0("clin_icd", i)
}
for (i in 1:20) {
  column_mappings[[paste0("RVSCODE", i)]] <- paste0("clin_rvs", i)
}

expected_types <- list(
  # Character columns (identifiers and date/time information)
  "character" = c(
    "CLAIM_SERIES_ID",
    "PSEUDO_CLAIMSERIES",
    "PIN",
    "PSEUDO_MEM_PIN",
    "HCI_PMCC_NO",
    "HCP_NO_LIST",
    "ADMISSION_DATE",
    "DATE_ADM",
    "ADMISSION_TIME",
    "TIME_ADM",
    "DISCHARGE_DATE",
    "DATE_DIS",
    "DISCHARGE_TIME",
    "TIME_DIS",
    "RECEIVE_DATE",
    "DATE_REC",
    "REFILE_DATE",
    "DATE_REF",
    "CHECK_DATE",
    "CHKDT",
    "EXTRACTION_DATE",
    "PRIMARY_ILLNESS",
    "SECONDARY_ILLNESS",
    "PAT_BDAY",
    "ICDCODES_ITEM7",
    "RVSCODES_ITEM7",
    paste0("ICDCODE", 1:12),
    paste0("RVSCODE", 1:20)
  ),

  # Integer columns (year, clinical, and patient data)
  "integer" = c(
    "ADMISSION_YEAR",
    "SRC_YR",
    "IS_ADMISSION_OPD",
    "IS_EMERGENCY_CASE",
    "OUT_PATIENT",
    "EMERGENCY",
    "PATIENT_AGE",
    "PATAGE"
  ),

  # Factor columns (categorical patient and claim information)
  "factor" = c(
    "PATIENT_TYPE",
    "PATIENT_RELATIONSHIP",
    "DEP_REL",
    "PATIENT_SEX",
    "PATSEX",
    "MEMCAT_PARENT_DESC",
    "MEMCAT_CHILD_DESC",
    "CLAIM_STATUS",
    "CLAIMS_STATUS",
    "PATIENT_DISPOSITION",
    "DISPOSITION",
    "ROOM_TYPE"
  ),

  # Numeric columns (claim amounts and patient weight)
  "numeric" = c(
    "CLAIM_PAID_AMOUNT",
    "CLAIMS_PAID_AMT",
    "CLAIM_AMOUNT_ACTUAL",
    "ACR_AMOUNT_ACTUAL",
    "PAT_BWT_KG"
  )
)

## COVID codes (for exclusion later)
covid_rvs <- c(
  "C19T1", "C19T2", "C19T3", "C19X1", "C19X2", "C19X3", "C19FRP",
  "C19IP1", "C19IP2", "C19IP3", "C19IP4", "C19PP1", "C19PP2",
  "C19PP3", "C19PP4", "MP01", "IMP02", "C19CI", "C19H1", "C19VIH",
  "C19VID"
)

## Convert the COVID codes into a regular expression pattern (without word boundaries)
covid_rvs_pattern <- paste(covid_rvs, collapse = "|")

# Initialize known values for each type of remapping
# Define known values for each column
known_values <- list(
  pat_type = c("MEMBER", "MM", "DEPENDENT", "DD"),
  claim_status = c("DENIED", "IN-PROCESS", "PAID", "RTH", "APRV4PAYMENT"),
  pat_memcat_parent = c("DIRECT CONTRIBUTOR", "INDIRECT CONTRIBUTOR"),
  pat_memcat_child = c(
    "EMPLOYED PRIVATE", "SELF-EARNING INDIVIDUAL", "SENIOR CITIZEN", "INDIGENT",
    "LIFETIME MEMBER", "SPONSORED", "MIGRANT WORKER", "EMPLOYED GOVERNMENT",
    "INFORMAL ECONOMY", "HOUSEHOLD HELP/KASAMBAHAY", "FOREIGN NATIONAL",
    "FILIPINOS WITH DUAL CITIZENSHIP / LIVING ABROAD", "SELF EARNING INDIVIDUAL",
    "FAMILY DRIVER", "FORMAL ECONOMY", "DIRECT CONTRIBUTOR", "PROFESSIONAL PRACTITIONER"
  ),
  clin_discharge = c(
    "IMPROVED", "RECOVERED", "HOME/DISCHARGED AGAINST MEDICAL ADVICE", "ABSCONDED",
    "TRANSFERRED/REFERRED", "EXPIRED", "UNDEFINED", "I", "R", "H", "A", "T", "E"
  )
)

# Define the fcase logic for remapping
remapped_column <- quote(fcase(
  dt[[column_name]] %in% c("MEMBER", "MM"), "M",
  dt[[column_name]] %in% c("DEPENDENT", "DD"), "D",
  dt[[column_name]] == "DENIED", "D",
  dt[[column_name]] == "IN-PROCESS", "I",
  dt[[column_name]] %in% c("PAID", "APRV4PAYMENT"), "G",
  dt[[column_name]] == "RTH", "R",
  dt[[column_name]] == "DIRECT CONTRIBUTOR", "D",
  dt[[column_name]] == "INDIRECT CONTRIBUTOR", "I",
  dt[[column_name]] == "EMPLOYED PRIVATE", "FORMAL",
  dt[[column_name]] == "SELF-EARNING INDIVIDUAL", "INFORMAL",
  dt[[column_name]] == "SENIOR CITIZEN", "SENIOR",
  dt[[column_name]] == "INDIGENT", "INDIGENT",
  dt[[column_name]] == "LIFETIME MEMBER", "LIFETIME",
  dt[[column_name]] == "SPONSORED", "SPONSORED",
  dt[[column_name]] %in% c("MIGRANT WORKER", "FOREIGN NATIONAL", "FILIPINOS WITH DUAL CITIZENSHIP / LIVING ABROAD"), "OFW",
  dt[[column_name]] %in% c("EMPLOYED GOVERNMENT", "HOUSEHOLD HELP/KASAMBAHAY", "FAMILY DRIVER", "FORMAL ECONOMY", "DIRECT CONTRIBUTOR"), "FORMAL",
  dt[[column_name]] == "PROFESSIONAL PRACTITIONER", "INFORMAL",
  dt[[column_name]] %in% c("IMPROVED", "RECOVERED", "I", "R"), "1", # Convert to string
  dt[[column_name]] %in% c("HOME/DISCHARGED AGAINST MEDICAL ADVICE", "H"), "2", # Convert to string
  dt[[column_name]] %in% c("ABSCONDED", "A"), "3", # Convert to string
  dt[[column_name]] %in% c("TRANSFERRED/REFERRED", "T"), "4", # Convert to string
  dt[[column_name]] %in% c("EXPIRED", "E"), "9", # Convert to string
  dt[[column_name]] == "UNDEFINED", NA_character_ # Ensure consistent type for missing values
))
### Helper functions for general data cleaning and processing


## NOTE: Consider renaming this to clean_string_column
clean_column <- function(column_to_clean, na_like_strings, neoplasms_dt = neoplasms_dt_actual) {
  ## Cleans a string column by performing basic string operations
  # column_to_clean: the column to clean.
  # na_like_strings: strings to treat as NA.
  # neoplasms_dt: data.table for neoplasm codes where slashes should be preserved.

  # Convert the column to uppercase and ASCII format
  column_to_clean <- as.character(column_to_clean)
  cleaned_col <- stri_trans_general(column_to_clean, "Latin-ASCII")
  cleaned_col <- toupper(cleaned_col)

  # Remove non-letter and non-digit characters from the string (except delimiters like commas and pipes)
  cleaned_col <- stri_replace_all_regex(cleaned_col, "[^\\w\\d,|]+", "")

  # Replace any NA-like strings (as defined) with actual NA values
  cleaned_col[cleaned_col %in% na_like_strings] <- NA_character_

  # Replace any COVID-related codes within the string with "COVID" (even if they are part of other codes)
  cleaned_col <- stri_replace_all_regex(cleaned_col, covid_rvs_pattern, "COVID")

  # Restore slashes for certain neoplasm ICD-10 codes, where slashes are important
  neopl <- setNames(neoplasms_dt$icd10, gsub("/", "", neoplasms_dt$icd10))
  matched_indices <- match(cleaned_col, names(neopl))
  cleaned_col[!is.na(matched_indices)] <- neopl[matched_indices[!is.na(matched_indices)]]

  # Return the cleaned column
  return(cleaned_col)
}

# collapse_columns <- function(cols_to_process, na_like_strings) {
#   ## Combines multiple string columns into one and cleans the result
#   # cols_to_process: a list of columns to concatenate.
#   # na_like_strings: strings considered as NA.

#   # Clean each column in cols_to_process by applying clean_column
#   cleaned_columns <- lapply(cols_to_process, function(col) {
#     clean_column(col, na_like_strings, neoplasms_dt)
#   })

#   # Collapse the cleaned columns into a single column, separated by "||"
#   collapsed_column <- do.call(paste, c(cleaned_columns, sep = "||"))

#   # Remove any occurrences of "||NA" or "NA||" or empty "||" from the collapsed string
#   collapsed_column <- stri_replace_all_regex(collapsed_column, "\\|\\|NA", "")
#   collapsed_column <- stri_replace_all_regex(collapsed_column, "NA\\|\\|", "")
#   collapsed_column <- stri_replace_all_regex(collapsed_column, "\\|\\|$", "")
#   collapsed_column <- stri_replace_all_regex(collapsed_column, "^\\|\\|", "")

#   # If the collapsed string is still NA-like, replace it with NA
#   collapsed_column <- ifelse(collapsed_column %in% na_like_strings,
#     NA_character_, collapsed_column
#   )

#   # Return the collapsed and cleaned column
#   return(collapsed_column)
# }

# Clean and collapse columns
collapse_columns <- function(
    cols_to_process,
    na_like_strings,
    neoplasms_dt = neoplasms_dt_actual) {
  ## Combines multiple string columns into one and cleans the result using clean_column.
  # cols_to_process: a list of columns to concatenate.
  # na_like_strings: strings considered as NA.
  # neoplasms_dt: data.table for neoplasm codes where slashes should be preserved.

  # Function to clean and split the column by different delimiters
  clean_and_split <- function(col, na_like_strings, neoplasms_dt) {
    # Clean the column using the clean_column function
    cleaned_col <- clean_column(col, na_like_strings, neoplasms_dt)

    # Split by multiple delimiters (comma, single pipe, or double pipe) while handling spaces
    split_col <- strsplit(cleaned_col, "\\s*,\\s*|\\|\\||\\|")

    # Return the split column
    return(split_col)
  }

  # Clean and split each column in cols_to_process
  cleaned_columns <- lapply(cols_to_process, function(col) {
    clean_and_split(col, na_like_strings, neoplasms_dt)
  })

  # Collapse the cleaned columns into a single column, combining them with "||"
  collapsed_column <- sapply(seq_along(cleaned_columns[[1]]), function(i) {
    # Combine corresponding rows from all columns and remove empty strings or NA-like values
    combined <- unique(unlist(lapply(cleaned_columns, function(col) col[[i]])))
    combined <- combined[!combined %in% na_like_strings & combined != ""]

    # Collapse the cleaned and combined values using "||" as the final separator
    if (length(combined) > 0) {
      return(paste(combined, collapse = "||"))
    } else {
      return(NA_character_)
    }
  })

  # Return the collapsed and cleaned column
  return(collapsed_column)
}

replace_empty_with_na_python <- function(dt, to_view_checks) {
  ## Replaces empty strings in a data.table with NA.
  ## Handles strings, factors, and lists.
  # dt: input data.table
  # to_view_checks: flag to track the replacement count for checks.

  # Identify columns that are characters, factors, or lists
  char_factor_cols <- names(dt)[sapply(
    dt,
    function(col) is.character(col) || is.factor(col) || is.list(col)
  )]

  # Create a summary table for tracking replacements
  replacement_summary <- data.table(
    Column = character(),
    Empty_Replaced = integer(),
    NA_Replaced = integer(),
    Character0_Replaced = integer()
  )

  # Loop through each identified column and replace empty strings
  for (col_name in char_factor_cols) {
    col <- dt[[col_name]]
    if (to_view_checks) {
      # Count how many empty, "NA", or "character(0)" entries exist
      empty_count <- sum(col == "", na.rm = TRUE)
      na_count <- sum(col == "NA", na.rm = TRUE)
      char0_count <- sum(col == "character(0)", na.rm = TRUE)
    }

    # Replace all empty, "NA", and "character(0)" values with actual NA
    dt[
      get(
        col_name
      ) == "" | get(col_name) == "NA" | get(col_name) == "character(0)",
      (col_name) := NA
    ]

    # If the column is a factor, make sure NA is a valid level
    if (is.factor(col)) {
      set(dt,
        j = col_name,
        value = factor(dt[[col_name]],
          levels = c(levels(col), NA)
        )
      )
    }

    if (to_view_checks) {
      # Update the replacement summary
      replacement_summary <- rbind(replacement_summary, data.table(
        Column = col_name,
        Empty_Replaced = empty_count,
        NA_Replaced = na_count,
        Character0_Replaced = char0_count
      ))
    }
  }

  if (to_view_checks) {
    # Filter out columns where no replacements were made
    replacement_summary <- replacement_summary[
      Empty_Replaced > 0 | NA_Replaced > 0 | Character0_Replaced > 0
    ]
  }

  return(
    list(
      # Return the modified data.table
      return_data = dt,
      # Return the summary of replacements
      return_replacement_summary = replacement_summary
    )
  )
}



replace_empty_with_na <- function(dt, to_view_checks = TRUE) {
  ## Replaces empty strings with NA across an entire data.table.
  # dt: input data.table
  # to_view_checks: flag to track the replacement count for checks.

  # Identify columns that are character, factor, or list
  char_factor_cols <- names(dt)[sapply(
    dt,
    function(col) is.character(col) || is.factor(col) || is.list(col)
  )]

  # Create a summary table for tracking replacements
  replacement_summary <- data.table(
    Column = character(),
    Empty_Replaced = integer(),
    NA_Replaced = integer(),
    Character0_Replaced = integer()
  )

  # Loop through each identified column
  for (col_name in char_factor_cols) {
    col <- dt[[col_name]]
    if (to_view_checks) {
      # Count how many empty, "NA", or "character(0)" entries exist
      empty_count <- sum(col == "", na.rm = TRUE)
      na_count <- sum(col == "NA", na.rm = TRUE)
      char0_count <- sum(col == "character(0)", na.rm = TRUE)
    }

    # Replace all empty, "NA", and "character(0)" values with actual NA
    dt[
      get(
        col_name
      ) == "" | get(col_name) == "NA" | get(col_name) == "character(0)",
      (col_name) := NA_character_
    ]

    # If the column is a factor, ensure that NA is a valid level
    if (is.factor(col)) {
      set(dt,
        j = col_name,
        value = factor(dt[[col_name]],
          levels = c(levels(col), NA)
        )
      )
    }

    if (to_view_checks) {
      # Update the replacement summary
      replacement_summary <- rbind(replacement_summary, data.table(
        Column = col_name,
        Empty_Replaced = empty_count,
        NA_Replaced = na_count,
        Character0_Replaced = char0_count
      ))
    }
  }

  if (to_view_checks) {
    # Filter out columns where no replacements were made
    replacement_summary <- replacement_summary[
      Empty_Replaced > 0 | NA_Replaced > 0 | Character0_Replaced > 0
    ]
  }

  return(
    list(
      # Return the modified data.table
      return_data = dt,
      # Return the summary of replacements
      return_replacement_summary = replacement_summary
    )
  )
}


replace_empty_with_none <- function(dt, to_view_checks = FALSE) {
  ## Replaces empty values and NA with "None" across a data.table
  # dt: input data.table
  # to_view_checks: flag to track the replacement count for checks.

  # Identify columns that are character, factor, or list
  char_factor_cols <- names(dt)[sapply(
    dt,
    function(col) is.character(col) || is.factor(col) || is.list(col)
  )]

  # Create a summary table for tracking replacements
  replacement_summary <- data.table(
    Column = character(),
    Empty_Replaced = integer(),
    NA_Replaced = integer(),
    Character0_Replaced = integer()
  )

  # Loop through each identified column
  for (col_name in char_factor_cols) {
    col <- dt[[col_name]]
    if (to_view_checks) {
      # Count how many empty, "NA", or "character(0)" entries exist
      empty_count <- sum(col == "", na.rm = TRUE)
      na_count <- sum(col == "NA", na.rm = TRUE)
      char0_count <- sum(col == "character(0)", na.rm = TRUE)
    }

    # Replace all empty, "NA", and "character(0)" values with "None"
    dt[
      get(
        col_name
      ) == "" | get(col_name) == "NA" | get(col_name) == "character(0)" | is.na(get(col_name)),
      (col_name) := "None"
    ]

    # If the column is a factor, ensure "None" is a valid level
    if (is.factor(col)) {
      set(dt,
        j = col_name,
        value = factor(dt[[col_name]],
          levels = c(levels(col), "None")
        )
      )
    }

    if (to_view_checks) {
      # Update the replacement summary
      replacement_summary <- rbind(replacement_summary, data.table(
        Column = col_name,
        Empty_Replaced = empty_count,
        NA_Replaced = na_count,
        Character0_Replaced = char0_count
      ))
    }
  }

  if (to_view_checks) {
    # Filter out columns where no replacements were made
    replacement_summary <- replacement_summary[
      Empty_Replaced > 0 | NA_Replaced > 0 | Character0_Replaced > 0
    ]
  }

  return(
    list(
      # Return the modified data.table
      return_data = dt,
      # Return the summary of replacements
      return_replacement_summary = replacement_summary
    )
  )
}

split_to_vector_single <- function(column) {
  ## Splits a column of strings into vectors using "|" as the delimiter
  # column: the column to split

  result <- lapply(column, function(x) {
    # If the entry is NA, leave it as is
    if (is.na(x)) {
      return(NA_character_)
    } else {
      # Split the string into a vector using "|"
      return(unlist(strsplit(x, "|", fixed = TRUE)))
    }
  })

  # Return the list of vectors
  return(result)
}

split_to_vector <- function(column) {
  ## Splits a column of strings into vectors using "||" as the delimiter
  # column: the column to split

  result <- lapply(column, function(x) {
    # If the entry is NA, leave it as is
    if (is.na(x)) {
      return(NA_character_)
    } else {
      # Split the string into a vector using "||"
      return(unlist(strsplit(x, "||", fixed = TRUE)))
    }
  })

  # Return the list of vectors
  return(result)
}

# collapse_and_clean_icd_rvs <- function(dt) {
#   ## Collapses and cleans ICD and RVS columns in a data.table
#   # dt: input data.table containing ICD and RVS columns

#   # Collapse the ICD codes from multiple columns into a single "clin_icd" column
#   dt[, clin_icd := collapse_columns(mget(paste0("clin_icd", 1:12)), na_like_strings)]
#   dt[, paste0("clin_icd", 1:12) := NULL] # Remove the individual columns

#   # Collapse the RVS codes from multiple columns into a single "clin_rvs" column
#   dt[, clin_rvs := collapse_columns(mget(paste0("clin_rvs", 1:20)), na_like_strings)]
#   dt[, paste0("clin_rvs", 1:20) := NULL] # Remove the individual columns

#   # Handle any lumped ICD codes by splitting them
#   dt[, clin_icd := remove_lumped_icd_codes(clin_icd)]

#   # Convert the cleaned columns into vectors
#   dt[, clin_icd := split_to_vector(clin_icd)]
#   # dt[, clin_rvs := remove_lumped_rvs_codes(clin_rvs)]
#   dt[, clin_rvs := split_to_vector(clin_rvs)]

#   # Return the cleaned data.table
#   return(dt)
# }


collapse_and_clean_icd_rvs <- function(dt) {
  ## Collapses and cleans ICD and RVS columns in a data.table
  # dt: input data.table containing ICD and RVS columns
  available_columns <- colnames(dt)

  # Dynamically detect which clin_icd columns exist
  icd_cols <- grep("^clin_icd\\d+$", available_columns, value = TRUE)
  if (length(icd_cols) > 0) {
    # Collapse the ICD codes, whether from multiple columns or a single column
    dt[, clin_icd := collapse_columns(mget(icd_cols), na_like_strings)]
    dt[, (icd_cols) := NULL] # Remove the individual columns after collapsing
  }

  # Dynamically detect which clin_rvs columns exist
  rvs_cols <- grep("^clin_rvs\\d+$", available_columns, value = TRUE)
  if (length(rvs_cols) > 0) {
    # Collapse the RVS codes, whether from multiple columns or a single column
    dt[, clin_rvs := collapse_columns(mget(rvs_cols), na_like_strings)]
    dt[, (rvs_cols) := NULL] # Remove the individual columns after collapsing
  }

  # Handle any lumped ICD codes by splitting them if clin_icd exists
  if ("clin_icd" %in% available_columns) {
    dt[, clin_icd := remove_lumped_icd_codes(clin_icd)] # Apply cleaning for lumped codes
    dt[, clin_icd := split_to_vector(clin_icd)] # Convert cleaned string to vector
  }

  # Handle any lumped RVS codes by splitting them if clin_rvs exists
  if ("clin_rvs" %in% available_columns) {
    # Assuming there is a `remove_lumped_rvs_codes` function, apply it here.
    # dt[, clin_rvs := remove_lumped_rvs_codes(clin_rvs)] #TODO: why is this commented out?
    dt[, clin_rvs := split_to_vector(clin_rvs)] # Convert cleaned string to vector
  }

  # Return the cleaned data.table
  return(dt)
}

clean_clinical_columns <- function(dt) {
  ## Cleans and processes the clinical columns in a data.table
  # dt: input data.table with clinical columns

  # Transfer ICD-10 codes from case rates to clinical ICD column
  # dt <- transfer_cr_icd(dt)
  # Deduplicate the ICD codes
  dt <- apply_add_c1_c2_to_clin_icd(dt)

  # TODO: append rvs to clin_proc, dont delete from c1 and c2
  # Process case rate 1 RVS codes
  c1_rvs_results <- append_and_remove_rvs(
    dt$clin_rvs, dt$c1, rvs_icd9
  )
  dt[, clin_rvs := c1_rvs_results$clin_rvs]
  dt[, c1 := c1_rvs_results$col]
  c1_discarded_rvs <- c1_rvs_results$discarded_rvs

  # Process case rate 2 RVS codes
  c2_rvs_results <- append_and_remove_rvs(
    dt$clin_rvs, dt$c2, rvs_icd9
  )
  dt[, clin_rvs := c2_rvs_results$clin_rvs]
  dt[, c2 := c2_rvs_results$col]
  c2_discarded_rvs <- c2_rvs_results$discarded_rvs

  # Ensure uniqueness of RVS codes in the final result
  # dt[, clin_rvs := lapply(clin_rvs, unique)]
  return(
    list(
      # Return the cleaned data.table
      dt = dt,
      # Return discarded RVS codes for checks
      discard_rvs_one = c1_discarded_rvs,
      discard_rvs_two = c2_discarded_rvs
    )
  )
}


transfer_cr_icd <- function(dt) {
  ## Transfers ICD-10 codes in case rates 1 and 2 to the clinical ICD list
  # dt: input data.table with case rates and clinical ICD codes

  # Split case rate 1 into vectors and transfer extra ICD-10 codes to clin_icd
  # dt[, c1 := split_to_vector(c1)]
  # c1_result <- transfer_extra_icd10s_to_clin_icd(
  #   dt$clin_icd, dt$c1
  # )
  # dt[, clin_icd := c1_result$clin_icd]
  # dt[, c1 := c1_result$col_first]

  # Split case rate 2 into vectors and transfer extra ICD-10 codes to clin_icd
  # dt[, c2 := split_to_vector(c2)]
  # c2_result <- transfer_extra_icd10s_to_clin_icd(
  #   dt$clin_icd, dt$c2
  # )
  # dt[, clin_icd := c2_result$clin_icd]
  # dt[, c2 := c2_result$col_first]

  # Return the updated data.table
  return(dt)
}

apply_add_c1_c2_to_clin_icd <- function(dt) {
  ## Deduplicates ICD codes in clin_icd if already present in c1 or c2
  # dt: input data.table

  # Add ICD codes from c1 and c2 to clin_icd, removing duplicates
  result <- add_c1_c2_to_clin_icd(dt$c1, dt$c2, dt$clin_icd)

  # Update clin_icd with the deduplicated result
  dt[, clin_icd := result$clin_icd]

  # Return the deduplicated data.table
  return(dt)
}
### Helper functions for remapping/reformatting key columns in the claims
remap_columns <- function(dt, column_name, to_view_checks = TRUE, known_values, remap_logic = remapped_column) {
  ## Function to remap different categorical columns in the claims dataset
  # dt: data.table
  # column_name: name of the column to remap
  # to_view_checks: boolean flag to enable check and capture of unmapped values
  # known_values: a list of known values for the specific column
  # remap_logic: quoted fcase logic passed as an argument

  # First, initialize the column with the original values to ensure no row count mismatch
  original_values <- dt[[column_name]]

  # Use `set` to modify the data.table by reference to avoid copying
  set(dt, j = column_name, value = eval(remap_logic))

  # Check for unmapped entries
  unknown_values <- setdiff(original_values[!is.na(original_values)], known_values[[column_name]])

  if (length(unknown_values) > 0 && to_view_checks) {
    warning(sprintf(
      "Unmapped values in column '%s': %s",
      column_name, paste(unknown_values, collapse = ", ")
    ))
  }

  return(
    list(
      data = dt,
      remapped = dt[[column_name]],
      unmapped = unknown_values
    )
  )
}

remap_patient_data <- function(dt, to_view_checks = TRUE) {
  ## Remap the categorical columns in the inpatient data

  # Initialize unmapped variables and mapped data tables
  pat_unmap <- parent_unmap <- child_unmap <- discharge_unmap <- claim_status_unmap <- NULL
  pat_mapped <- parent_mapped <- child_mapped <- discharge_mapped <- claim_status_mapped <- NULL

  # Define the columns that need remapping
  columns_to_remap <- list(
    pat_type = "pat_type",
    pat_memcat_parent = "pat_memcat_parent",
    pat_memcat_child = "pat_memcat_child",
    clin_discharge = "clin_discharge",
    claim_status = "claim_status"
  )

  # Apply remapping for each column
  for (col_name in names(columns_to_remap)) {
    result <- remap_columns(dt, columns_to_remap[[col_name]], to_view_checks, known_values, remapped_column)

    # Update the original column using `set`
    set(dt, j = columns_to_remap[[col_name]], value = result$remapped)

    # Capture mapped and unmapped values
    if (col_name == "pat_type") {
      pat_mapped <- unique(data.table(Original = result$remapped, Mapped = result$remapped))
      if (length(result$unmapped) > 0 && to_view_checks) pat_unmap <- result$unmapped
    } else if (col_name == "pat_memcat_parent") {
      parent_mapped <- unique(data.table(Original = result$remapped, Mapped = result$remapped))
      if (length(result$unmapped) > 0 && to_view_checks) parent_unmap <- result$unmapped
    } else if (col_name == "pat_memcat_child") {
      child_mapped <- unique(data.table(Original = result$remapped, Mapped = result$remapped))
      if (length(result$unmapped) > 0 && to_view_checks) child_unmap <- result$unmapped
    } else if (col_name == "clin_discharge") {
      discharge_mapped <- unique(data.table(Original = result$remapped, Mapped = result$remapped))
      if (length(result$unmapped) > 0 && to_view_checks) discharge_unmap <- result$unmapped
    } else if (col_name == "claim_status") {
      claim_status_mapped <- unique(data.table(Original = result$remapped, Mapped = result$remapped))
      if (length(result$unmapped) > 0 && to_view_checks) claim_status_unmap <- result$unmapped
    }
  }

  # Return the remapped data and all mapping/unmapped data
  return(
    list(
      data = dt,
      pat_type_mapped = pat_mapped,
      pat_memcat_parent_mapped = parent_mapped,
      pat_memcat_child_mapped = child_mapped,
      clin_discharge_mapped = discharge_mapped,
      claim_status_mapped = claim_status_mapped,
      pat_type_unmapped = pat_unmap,
      memcat_parent_unmapped = parent_unmap,
      memcat_child_unmapped = child_unmap,
      discharge_unmapped = discharge_unmap,
      claim_status_unmapped = claim_status_unmap
    )
  )
}

remove_lumped_icd_codes <- function(column) {
  ## Takes a column and separates out ICD-10 codes using "||"
  ## been lumped into a single string

  # Use regex to add "||" between letters and digits in the ICD codes (e.g., A123B456 -> A123||B456)
  modified_column <- stri_replace_all_regex(
    column, "(?<=\\d)(?=[A-Za-z])", "||",
    opts_regex = stri_opts_regex() # Specify regex options for the replacement
  )

  # Return the modified column with ICD codes split
  return(modified_column)
}

remove_lumped_rvs_codes <- function(column) {
  ## Separates out lumped RVS codes by splitting into chunks of 5 chars each

  # Define a helper function to split each code into 5-character chunks
  split_rvs_codes_helper <- function(code) {
    if (is.na(code) || code == "" || is.null(code)) {
      return(NA_character_) # If the input code is NA, empty, or NULL, return NA
    }

    # Remove all non-alphanumeric characters and clean the code
    code_clean <- gsub("\\|", "", code) # Remove all "|" characters
    code_clean <- gsub("[^A-Z0-9]", "", code_clean) # Remove non-alphanumeric characters

    # If the cleaned code length is 0, return NA
    if (nchar(code_clean) == 0) {
      return(NA_character_)
    } else if (nchar(code_clean) %% 5 != 0) {
      # If the length is not a multiple of 5, log a message and return NA
      message(paste0("Total length of concatenated RVS codes is not a multiple of 5 characters: ", code_clean))
      return(NA_character_)
    } else {
      # Insert "||" every 5 characters to split the code
      modified_code <- gsub("(.{5})", "\\1||", code_clean)

      # Remove trailing "||" if present
      modified_code <- gsub("\\|\\|$", "", modified_code)

      return(modified_code)
    }
  }

  # Apply the helper function to each element of the input column
  modified_column <- sapply(as.character(column), split_rvs_codes_helper, USE.NAMES = FALSE)

  return(modified_column) # Return the modified column with split RVS codes
}

remove_lumped_icd9_codes <- function(column) {
  ## Separates out lumped ICD9 codes by splitting into chunks of 4 chars each

  # Define a helper function to process each code
  split_rvs_codes_helper_icd9 <- function(code) {
    if (is.na(code) || code == "" || is.null(code)) {
      return(NA_character_) # Return NA if input is NA, empty, or NULL
    }

    # Remove all '|' characters and ensure only alphanumeric characters are kept
    code_clean <- gsub("\\|", "", code) # Remove all "|" characters
    code_clean <- gsub("[^A-Z0-9]", "", code_clean) # Remove non-alphanumeric characters

    # If the cleaned code length is 0, return NA
    if (nchar(code_clean) == 0) {
      return(NA_character_)
    } else if (nchar(code_clean) %% 4 != 0) {
      # If the length is not a multiple of 4, log a message and return NA
      message(paste0("Total length of concatenated ICD9 codes is not a multiple of 4 characters: ", code_clean))
      return(NA_character_)
    } else {
      # Insert "||" every 4 characters to split the code
      modified_code <- gsub("(.{4})", "\\1||", code_clean)

      # Remove trailing "||" if present
      modified_code <- gsub("\\|\\|$", "", modified_code)

      return(modified_code)
    }
  }

  # Apply the helper function to each element in the input column
  modified_column <- sapply(as.character(column), split_rvs_codes_helper_icd9, USE.NAMES = FALSE)

  return(modified_column) # Return the modified column with split ICD9 codes
}


transfer_extra_icd10s_to_clin_icd <- function(clin_icd, col) {
  ## Transfer extra ICD-10 codes from a column to the compilation of ICD-10 codes for the case
  # clin_icd: column with all the ICD-10 codes
  # col: column with the possible extra ICD-10 codes

  # If clin_icd is null, initialize it as an empty vector
  clin_icd <- lapply(clin_icd, function(x) if (is.null(x)) character() else x)

  # Extract the first element of col (typically case rate c1 or c2) to keep separately
  col_first <- lapply(col, function(x) x[1])

  # Append the remaining elements of col to clin_icd for each row, only if not already in clin_icd
  clin_icd <- mapply(function(icd, c1) {
    extra_icds <- c1[-1] # Take all elements from c1 except the first
    # Only append elements that are not already in clin_icd
    new_icds <- extra_icds[!extra_icds %in% icd]
    c(icd, new_icds) # Concatenate clin_icd with the filtered new elements
  }, clin_icd, col, SIMPLIFY = FALSE)

  # Return the updated clin_icd and the first element of col
  return(list(clin_icd = clin_icd, col_first = col_first))
}


get_unique_icd_codes <- function(c1, c2, clin_icd) {
  ## Obtains list of all unique ICD-10 codes across all cases and columns

  # Concatenate all elements from c1, c2, and clin_icd and remove duplicates using unique
  icds <- unique(c(unlist(c1), unlist(c2), unlist(clin_icd)))

  # Remove any NA values from the list of ICD codes
  icds <- icds[!is.na(icds)]

  # Return the unique list of ICD codes
  return(icds)
}


create_thai_icd10_environment <- function(thai_icd10_codes) {
  ## Create environment for Thai ICD-10 codes

  # Create a new environment where the Thai ICD-10 codes are set to TRUE
  thai_icd10_env <- list2env(
    setNames(as.list(rep(TRUE, length(thai_icd10_codes))), thai_icd10_codes)
  )

  # Return the created environment
  return(thai_icd10_env)
}


find_direct_icd_matches <- function(icds, thai_icd10_env) {
  ## Identify ICD-10 codes with exact matches in the Thai ICD-10 library
  # icds: list of ICD-10 codes to be cross-checked
  # thai_icd10_env: environment of Thai ICD-10 codes

  # Retrieve the values of each ICD code from the thai_icd10_env environment.
  # If a code is not found, it returns FALSE (using ifnotfound argument).
  direct_matches <- mget(icds, thai_icd10_env, ifnotfound = as.list(rep(FALSE, length(icds))))

  # Extract the names of ICD codes that matched (i.e., returned TRUE from the environment)
  direct_match_codes <- names(unlist(direct_matches[unlist(direct_matches) == TRUE]))

  # Return the matched ICD codes
  return(direct_match_codes)
}

generate_icd10_mapping <- function(icds, thai_icd10_env, neoplasms_env) {
  ## Map ICD-10 codes to their closest equivalents in the Thai ICD-10 library

  icd_mapping <- list() # Initialize an empty list to store mappings
  modified_count <- 0 # Initialize counter for modified codes

  # Loop through each ICD-10 code to generate mappings
  for (d in icds) {
    d <- str_trim(d) # Trim whitespace from the code

    # If the code has an exact match, map it directly
    if (exists(d, thai_icd10_env)) {
      icd_mapping[[d]] <- d
    } else if (!exists(d, neoplasms_env) && grepl("[A-Za-z]", d) && grepl("[0-9]", d)) {
      # If it's not a neoplasm and contains both letters and numbers, modify it
      if (nchar(d) == 3 && exists(paste0(d, "9"), thai_icd10_env)) {
        # If the code is 3 characters long, try appending "9"
        icd_mapping[[d]] <- paste0(d, "9")
        modified_count <- modified_count + 1
      } else if (nchar(d) >= 4) {
        # Try trimming digits from the end to find a match
        for (i in seq_len(nchar(d) - 3)) {
          new_d <- substr(d, 1, nchar(d) - i)
          if (exists(new_d, thai_icd10_env)) {
            icd_mapping[[d]] <- new_d
            modified_count <- modified_count + 1
            break
          }
        }
      }
    }
  }

  # Return the mapping and count of modified codes
  return(list(icd_mapping = icd_mapping, modified_count = modified_count))
}


apply_icd10_mapping_to_columns <- function(c1, c2, clin_icd, icd10_env) {
  ## Maps ICD-10 codes to the given columns using the provided environment

  # Helper function to map ICD-10 codes using the provided environment
  map_icd10_helper <- function(codes) {
    # Use mget to map each code to its equivalent in icd10_env or return the original if no match is found
    mapped <- mget(codes, icd10_env, ifnotfound = as.list(codes))
    return(unname(unlist(mapped))) # Return the mapped codes as an unnamed vector
  }

  # Apply the mapping function to each of the columns (c1, c2, and clin_icd)
  # Cel: change this to a for-loop
  # Carlos: lapply is faster because lapply is optimized for iteration in R’s internal C/C++ code,
  # Carlos: whereas for loops have more overhead due to their explicit nature in R.
  c1_mapped <- lapply(c1, map_icd10_helper)
  c2_mapped <- lapply(c2, map_icd10_helper)
  clin_icd_mapped <- lapply(clin_icd, map_icd10_helper)

  # Return the mapped values for c1, c2, and clin_icd
  return(list(c1 = c1_mapped, c2 = c2_mapped, clin_icd = clin_icd_mapped))
}

add_c1_c2_to_clin_icd <- function(c1, c2, clin_icd) {
  ## Adds c1 and c2 ICD codes to clin_icd, allowing duplicates

  # Create a data.table to handle the merging of codes efficiently
  datatable <- data.table(c1 = c1, c2 = c2, clin_icd = clin_icd)

  # Map function to concatenate clin_icd with c1 and c2, allowing duplicates
  # TODO: Don't duplicate it if it's already there
  datatable[, clin_icd := Map(function(c1, c2, icd) {
    c(icd, c1, c2) # Concatenate clin_icd with c1 and c2
  }, c1, c2, clin_icd)]

  # Return the updated clin_icd column
  return(list(clin_icd = datatable$clin_icd))
}

split_rvs_codes <- function(rvs_icd9) {
  ## Split the RVS codes into those with DRG and those without

  # Filter rows where the RVS code has an associated DRG
  with_drg <- rvs_icd9[is_drg == TRUE]

  # Filter rows where the RVS code does not have a DRG
  without_drg <- rvs_icd9[!rvs %in% with_drg$rvs]

  # Return the lists of RVS codes with and without DRG
  return(list(with_drg = with_drg, without_drg = without_drg))
}


create_rvs_map_lists <- function(with_drg) {
  ## Create two lists for mapping RVS codes to ICD-9-CM codes

  # Order the table by RVS code and whether it's associated with a DRG
  setorder(with_drg, rvs, -is_drg)

  # Group by RVS code and create a list of associated ICD-9-CM codes for each RVS
  unique_rvs <- with_drg[, .(icd9cm_list = list(icd9cm)), by = rvs]

  # Separate RVS codes that map to a single ICD-9-CM code from those with multiple mappings
  solo <- unique_rvs[lengths(icd9cm_list) == 1]
  list_mapped <- unique_rvs[lengths(icd9cm_list) > 1]

  # Create named lists for solo and multi-mapped RVS codes
  rvs_map_solo <- setNames(solo$icd9cm_list, solo$rvs)
  rvs_map_list <- setNames(list_mapped$icd9cm_list, list_mapped$rvs)

  # Return the solo and multi-mapped lists
  return(list(rvs_map_list = rvs_map_list, rvs_map_solo = rvs_map_solo))
}


get_icd9_codes <- function(clin_rvs, rvs_map_solo_env) {
  ## Maps a column containing RVS codes to ICD-9-CM

  # Use lapply to loop through each row of clin_rvs
  lapply(clin_rvs, function(x) {
    codes <- unlist(x) # Unlist the RVS codes in each row

    # Map all codes to ICD-9-CM equivalents using the rvs_map_solo_env environment
    mappable <- codes[!is.na(mget(codes, envir = rvs_map_solo_env, ifnotfound = NA_character_))]

    if (length(mappable) > 0) {
      # Return the unique set of mapped ICD-9 codes
      unique(unlist(mget(mappable, envir = rvs_map_solo_env)))
    } else {
      NA_character_ # Return NA if no mappable codes are found
    }
  })
}

find_and_append_valid_rvs <- function(datatable, valid_rvs_codes) {
  datatable[, matches := lapply(col, function(x) {
    # Find valid RVS codes within each vector of 'col'
    valid_codes <- x[x %in% valid_rvs_codes]
    return(unique(valid_codes))
  })]

  # Append valid matches to the existing 'clin_rvs' vector
  datatable[, clin_rvs := mapply(function(rvs, matches) unique(c(rvs, matches)), clin_rvs, matches, SIMPLIFY = FALSE)]
}

remove_5_digit_codes <- function(col) {
  # Handle the column as a list of vectors
  return(lapply(col, function(x) {
    if (is.na(x)) {
      return(NA_character_)
    } else {
      return(x)
    }
    # stri_replace_all_regex(x, "\\b\\d{5}\\b", "") # Don't delete rvs codes from c1 and c2
  }))
}

warn_invalid_rvs <- function(matches, valid_rvs_codes) {
  ## Triggers warnings for invalid RVS codes

  # Create a new environment for valid RVS codes
  valid_rvs_env <- new.env(hash = TRUE, parent = emptyenv())

  # Populate the environment with valid RVS codes
  for (code in valid_rvs_codes) {
    assign(code, TRUE, envir = valid_rvs_env)
  }

  # Identify invalid RVS codes by checking if they exist in the valid_rvs_env environment
  invalid_matches <- lapply(matches, function(x) x[!vapply(x, exists, logical(1), envir = valid_rvs_env)])

  # Flatten the list of invalid matches into a single vector
  discarded_codes <- unlist(invalid_matches)

  # If invalid codes exist, create a summary table of their counts
  if (length(discarded_codes) > 0) {
    discarded_table <- data.table(CODE = discarded_codes)[, .N, by = CODE][order(-N)]
    setnames(discarded_table, c("CODE", "count"))
  } else {
    discarded_table <- data.table()
  }

  # Return the table of invalid codes and their counts
  return(discarded_table)
}


append_and_remove_rvs <- function(clin_rvs, col, rvs_icd9) {
  ## Ensure both clin_rvs and col are lists of vectors
  datatable <- data.table(clin_rvs = clin_rvs, col = col)

  valid_rvs_codes <- rvs_icd9$rvs

  # Append valid RVS codes to clin_rvs (handling each element of the vectors)
  find_and_append_valid_rvs(datatable, valid_rvs_codes)

  # Modify the column by removing 5-digit codes from each vector and recursively unlisting
  datatable[, col := lapply(col, function(x) {
    # Optimize by checking if x is already a character vector
    cleaned_col <- if (is.character(x)) {
      remove_5_digit_codes(x) # Directly modify the character vector
    } else {
      # Recursively unlist and clean the elements
      remove_5_digit_codes(as.character(x))
    }
    return(unlist(cleaned_col))
  })]

  # Trigger warnings for invalid RVS codes
  discarded_rvs <- warn_invalid_rvs(datatable$matches, valid_rvs_codes)

  # Return updated clin_rvs, cleaned col, and invalid codes
  return(list(clin_rvs = datatable$clin_rvs, col = datatable$col, discarded_rvs = discarded_rvs))
}

apply_find_pdx <- function(c1, c2, clin_icd, acc_pdx) {
  ## Function to apply the PDX finding logic in a vectorized manner

  # Step 1: Create a new environment for accepted PDX codes
  acc_pdx_env <- new.env(hash = TRUE, parent = emptyenv())

  # Step 2: Populate the environment with accepted PDX codes
  for (code in acc_pdx) {
    assign(code, TRUE, envir = acc_pdx_env)
  }

  # Step 3: Define helper function to find PDX for each row
  find_pdx_for_row <- function(c1, c2, clin_icd) {
    # Function to check similarity between two strings
    check_similarity <- function(x, y) {
      score <- 0
      min_len <- min(nchar(x), nchar(y))
      for (i in 1:min_len) {
        if (substr(x, i, i) == substr(y, i, i)) {
          score <- score + 1
        }
      }
      return(score)
    }

    # Split c1 and c2 by '|' if necessary
    c1 <- unlist(strsplit(c1, "\\|"))
    c2 <- unlist(strsplit(c2, "\\|"))

    # Step 1: Check if any element in c1 or c2 is an acceptable PDx
    for (cr_list in list(c1, c2)) {
      for (cr in cr_list) {
        if (!is.na(cr) && exists(cr, envir = acc_pdx_env)) {
          return(list(pdx = cr, pdx_code = ifelse(cr %in% c1, 1, 2)))
        }
      }
    }

    # Step 2: Unlist clin_icd by splitting if necessary
    clin_icd <- unlist(strsplit(clin_icd, "\\|"))

    # Step 3: Get a list of acceptable PDx from clin_icd
    pdxs <- unique(clin_icd)
    pdxs <- pdxs[sapply(pdxs, function(x) exists(x, envir = acc_pdx_env))]

    # Step 4: Handle cases with no or only one acceptable PDx
    if (length(pdxs) == 0) {
      return(list(pdx = NA_character_, pdx_code = 99))
    } else if (length(pdxs) == 1) {
      return(list(pdx = pdxs[1], pdx_code = 3))
    }

    # Step 5: Check c1 and c2 for matching starting letters
    for (cr_list in list(c1, c2)) {
      for (cr in cr_list) {
        if (!is.na(cr)) {
          starting_letter <- substr(cr, 1, 1)
          starting_codes <- pdxs[substr(pdxs, 1, 1) == starting_letter]

          if (length(starting_codes) == 1) {
            return(list(pdx = starting_codes[1], pdx_code = 4))
          } else if (length(starting_codes) > 1) {
            starting_codes <- starting_codes[
              order(sapply(starting_codes, function(x) check_similarity(cr, x)), decreasing = TRUE)
            ]
            return(list(pdx = starting_codes[1], pdx_code = 5))
          }
        }
      }
    }

    # Step 6: If no matching starting letter, pick a random PDx
    if (length(pdxs) > 0) {
      return(list(pdx = sample(pdxs, 1), pdx_code = 6))
    }

    # Step 7: Return NA and code 99 if no PDx is found
    return(list(pdx = NA_character_, pdx_code = 99))
  }

  # Step 4: Apply find_pdx_for_row function to all rows
  result <- mapply(find_pdx_for_row, c1, c2, clin_icd, SIMPLIFY = FALSE)

  # Step 5: Extract PDX and PDX codes into vectors
  pdx <- sapply(result, function(x) x$pdx)
  pdx_code <- sapply(result, function(x) x$pdx_code)

  # Return the PDX values and codes
  return(list(pdx = pdx, pdx_code = pdx_code))
}

generate_dob <- function(bdays, ages, date_adms) {
  ## Function to generate dates of birth (DOB) based on birthdates, ages, and admission dates

  set.seed(global_seed) # Ensure reproducibility by setting a global seed

  require(lubridate) # Load lubridate for date manipulation

  # Convert ages to numeric
  ages <- as.numeric(ages)

  # Initialize DOB vector with NA values
  dob <- rep(NA_character_, length(ages))

  ## Step 1: Use provided birthdates where available
  valid_bdays_indices <- !is.na(bdays) & bdays != "" # Find valid birthdate indices

  # Convert valid birthdates to desired format
  dob[valid_bdays_indices] <- format(ymd(bdays[valid_bdays_indices]), "%d/%m/%Y")

  ## Step 2: Handle missing birthdates
  missing_bday_indices <- which(is.na(bdays) | bdays == "") # Find indices with missing birthdates
  ref_dates <- ymd(date_adms[missing_bday_indices]) # Get reference dates (admission dates)

  ## Step 3: Handle age == 0
  zero_age_indices <- which(!is.na(ages[missing_bday_indices]) & ages[missing_bday_indices] == 0)

  # Generate random days for age 0 cases
  if (length(zero_age_indices) > 0) {
    dob[missing_bday_indices[zero_age_indices]] <- format(
      ref_dates[zero_age_indices] - days(sample(1:27, length(zero_age_indices), replace = TRUE)), "%d/%m/%Y"
    )
  }
  # TODO for where birthdate exists impute it as the difference between date admission and birthdate
  # Otherwise just "3"

  ## Step 4: Handle positive ages
  positive_age_indices <- which(!is.na(ages[missing_bday_indices]) & ages[missing_bday_indices] > 0)

  # Subtract exact age in years from the reference date
  if (length(positive_age_indices) > 0) {
    truncated_ages <- floor(ages[missing_bday_indices][positive_age_indices])
    dob[missing_bday_indices[positive_age_indices]] <- format(
      ref_dates[positive_age_indices] - years(truncated_ages), "%d/%m/%Y"
    )
  }

  # Return the vector of generated DOBs
  return(dob)
}
format_large_numbers <- function(x) {
  #' @title Format Large Numbers
  #'
  #' @description This function formats large numbers into a
  #' more readable string with units (k, m, b).
  #'
  #' @param x numeric. The number to be formatted.
  #'
  #' @return character. The formatted number as a string.

  if (x >= 1e9) {
    return(sprintf("%.1fb", x / 1e9))
  } else if (x >= 1e6) {
    return(sprintf("%.1fm", x / 1e6))
  } else if (x >= 1e3) {
    return(sprintf("%.1fk", x / 1e3))
  } else {
    return(as.character(x))
  }
}

# Function to print time estimates
print_time_estimates <- function() {
  #' @title Print Time Estimates
  #'
  #' @description This function prints time estimates for
  #' processing rows in a data table.
  #'
  #' @return NULL.
  toc_data <- toc(log = TRUE)
  total_time <- toc_data$toc - toc_data$tic
  total_rows <- if (to_sample) {
    dim_dt[1] * split_parts * sample_size_divisor
  } else {
    dim_dt[1] * split_parts
  }

  total_rows_dt <- dim_dt[1] * split_parts
  # total_cells <- dim_dt[1] * dim_dt[2]
  # time_per_cell <- total_time / total_cells
  time_per_row <- total_time / total_rows_dt
  time_estimate_total_rows <- time_per_row * total_rows

  # Format the row numbers
  formatted_total_rows_dt <- format_large_numbers(total_rows_dt)
  formatted_total_rows <- format_large_numbers(total_rows)

  # Print the results for processing the whole file
  # cat(sprintf(
  #   "Time spent (total) for %s rows: %2.2f sec  (actual)\n",
  #   formatted_total_rows_dt, total_time
  # ))
  cat(sprintf(
    "Time spent (t/row) for %s rows: %2.2f msec\n",
    formatted_total_rows_dt, time_per_row * 1000
  ))
  cat(sprintf(
    "Time (est) (total) for %s rows: %2.2f min\n",
    formatted_total_rows, time_estimate_total_rows / 60
  ))
}

# Function to print status updates using lubridate
print_status_update <- function(status_part, split_parts, processing_times, phase) {
  #' @title Print Status Update
  #' @description Print the status update and estimated time remaining.
  #' @param status_part integer. The current status_part number.
  #' @param split_parts integer. Total number of parts.
  #' @param processing_times numeric. Array of processing times for each
  #' status_part.

  # Calculate elapsed time and averages
  elapsed_time <- sum(processing_times[1:status_part])
  avg_time_per_part <- elapsed_time / status_part
  estimated_total_time <- avg_time_per_part * split_parts
  estimated_remaining_time <- estimated_total_time - elapsed_time

  # Convert time to period (using lubridate)
  convert_to_hr_min_sec <- function(seconds) {
    # Round seconds to the nearest whole number
    period <- seconds_to_period(round(seconds))
    return(period)
  }

  # Calculate elapsed and remaining time
  elapsed <- convert_to_hr_min_sec(elapsed_time)
  remaining <- convert_to_hr_min_sec(estimated_remaining_time)

  # Format period to string
  format_time <- function(period) {
    # Extract components
    h <- hour(period)
    m <- minute(period)
    s <- second(period)

    # Construct time string with labels
    time_components <- c()
    if (h > 0) time_components <- c(time_components, paste0(h, "h"))
    if (m > 0 || h > 0) time_components <- c(time_components, paste0(m, "m"))
    time_components <- c(time_components, paste0(s, "s"))

    # Join components and return
    time_str <- paste(time_components, collapse = " ")
    return(trimws(time_str))
  }

  elapsed_str <- format_time(elapsed)
  remaining_str <- format_time(remaining)

  # Determine when to print the status update
  if (phase == "split") {
    if (avg_time_per_part >= 4) {
      # Print status updates for every status_part
      cat(sprintf(
        "\rFinished splitting %d of %d parts in %s (ETA %s)       ",
        status_part, split_parts, elapsed_str, remaining_str
      ))
      flush.console()
    } else if (avg_time_per_part < 4 && status_part %% 5 == 0) {
      # Print status updates for every 5th, 10th, 15th status_part
      cat(sprintf(
        "\rFinished splitting %d of %d parts in %s (ETA %s)       ",
        status_part, split_parts, elapsed_str, remaining_str
      ))
      flush.console()
    }
  } else if (phase == "clean") {
    if (avg_time_per_part >= 4) {
      # Print status updates for every status_part
      cat(sprintf(
        "\rFinished cleaning %d of %d parts in %s (ETA %s)       ",
        status_part, split_parts, elapsed_str, remaining_str
      ))
      flush.console()
    } else if (avg_time_per_part < 4 && status_part %% 5 == 0) {
      # Print status updates for every 5th, 10th, 15th status_part
      cat(sprintf(
        "\rFinished cleaning %d of %d parts in %s (ETA %s)       ",
        status_part, split_parts, elapsed_str, remaining_str
      ))
      flush.console()
    }
  }
  
}

concatenate_r_files <- function(input_path, output_file) {
  #' @title Concatenate R Files
  #'
  #' @description This function concatenates all .R files in
  #' a specified directory into a single output file.
  #'
  #' @param input_path character. The directory containing the
  #' .R files to concatenate.
  #' @param output_file character. The path to the output file
  #' where the concatenated content will be written.
  #'
  #' @return NULL.

  # List all .R files in the directory
  r_files <- list.files(
    path = input_path,
    pattern = "\\.R$", full.names = TRUE
  )

  # Delete the existing output file if it exists
  if (file.exists(output_file)) {
    file.remove(output_file)
  }

  # Read and concatenate contents
  file_contents <- lapply(r_files, readLines)
  concatenated_content <- unlist(file_contents)

  # Write concatenated content to the output file
  cat(concatenated_content, file = output_file, sep = "\n")
}
print_summary_tables <- function(final_combined_summaries, end_nrow) {
  #' @title Print Summary Tables
  #'
  #' @description This function prints summary tables for a given dataset.
  #'
  #' @param final_combined_summaries list. The final combined
  #' summaries to be printed.
  #' @param end_nrow integer. The number of rows to show
  #' in the summary tables.
  #'
  #' @return NULL. Prints the summary tables.

  summary <- final_combined_summaries

  cat("\nRename Success:\n", summary$final_rename_success, "")

  final_icd_replacements <- unique(rbind(
    summary$final_ICD_replacements_1,
    summary$final_ICD_replacements_2
  ))

  if (nrow(final_icd_replacements) > 0) {
    print(kable(head(final_icd_replacements, end_nrow),
      format = "markdown",
      caption = "ICD Normalized Text for clin c1 & c2 Before Splitting"
    ))
  } else {
    cat(
      sprintf(
        "\nNo ICD replacements found in clin c1 & c2 with diff chars > %d",
        diff_chars
      ),
      "\nNote: commas, asterisks, plus signs, and whitespaces are ignored.\n"
    )
  }

  # Display unique before and after mappings for each categorical variable
  display_unique_mappings <- function(mapped_data, mapping_name, tmp_nrow) {
    #' @title Display Unique Mappings
    #'
    #' @description Displays the unique before-and-after mappings
    #' for a given dataset.
    #'
    #' @param mapped_data data.table. The data table with Original
    #' and Mapped columns.
    #' @param mapping_name character. The name of the mapping being displayed.
    #' @param tmp_nrow integer. Number of rows to display in the output.
    #'
    #' @return NULL. Prints the unique mappings.

    # Ensure the data has the correct columns
    if (
      !("Original" %in% names(mapped_data)) ||
        !("Mapped" %in% names(mapped_data))) {
      stop("The data table must contain 'Original' and 'Mapped' columns.")
    }

    # Create a data table to display unique before and after mappings
    unique_mappings <- unique(mapped_data)

    # Print the mappings using kable
    print(kable(head(unique_mappings, tmp_nrow),
      format = "markdown",
      caption = sprintf("Unique Before and After Mappings for %s", mapping_name)
    ))
  }

  display_unique_mappings(
    summary$final_pat_type_mapped, "Patient Type", tmp_nrow
  )
  display_unique_mappings(
    summary$final_memcat_parent_mapped, "Memcat Parent", tmp_nrow
  )
  display_unique_mappings(
    summary$final_memcat_child_mapped, "Memcat Child", tmp_nrow
  )
  display_unique_mappings(
    summary$final_clin_discharge_mapped, "Discharge", tmp_nrow
  )
  display_unique_mappings(
    summary$final_claim_status_mapped, "Claim Status", tmp_nrow
  )

  final_discard_rvs <- rbind(
    summary$final_discard_rvs_one,
    summary$final_discard_rvs_two
  )[, .(count = sum(count)), by = CODE][order(-count)]

  if (nrow(final_discard_rvs) > 0) {
    print(kable(head(final_discard_rvs, end_nrow),
      format = "markdown",
      caption = "Discarded RVS Codes"
    ))
  } else {
    cat("\nNo RVS codes discarded.\n\n")
  }

  if (nrow(summary$final_empty_strings_replaced_0) > 0) {
    print(kable(
      head(
        summary$final_empty_strings_replaced_0, end_nrow
      ),
      format = "markdown",
      caption = "Empty Strings Replaced (Zeroth Set)"
    ))
  } else {
    cat("\nNo empty strings replaced in the zeroth set.\n\n")
  }

  if (nrow(summary$final_empty_strings_replaced_1) > 0) {
    print(kable(
      head(
        summary$final_empty_strings_replaced_1, end_nrow
      ),
      format = "markdown",
      caption = "Empty Strings Replaced (First Set)"
    ))
  } else {
    cat("\nNo empty strings replaced in the first set.\n\n")
  }

  if (nrow(summary$final_empty_strings_replaced_2) > 0) {
    print(kable(
      head(
        summary$final_empty_strings_replaced_2, end_nrow
      ),
      format = "markdown",
      caption = "Empty Strings Replaced (Second Set)"
    ))
  } else {
    cat("\nNo empty strings replaced in the second set.\n\n")
  }

  cat(
    sprintf(
      "There are %d RVS codes without an ICD-9CM",
      summary$final_without_drg
    ), "equivalent recognized by the TDRG ICD9CM\n"
  )
  cat(
    sprintf(
      "There are %d unique RVS codes that appear in the claims.\n",
      summary$final_rvss
    )
  )
  cat(
    sprintf(
      "Of these, %d (%.2f%%) have a mapping to an ICD-9-CM code.\n",
      summary$final_mappable_rvs,
      (summary$final_mappable_rvs /
        summary$final_rvss) * 100
    )
  )
  cat(
    sprintf(
      "Of these, there are %d (%.2f%%)",
      summary$final_multi_mapped_rvs,
      (summary$final_multi_mapped_rvs /
        summary$final_rvss) * 100
    ), "with more than one ICD9 equivalent",
    "recognized by the Thai ICD9 library.\n"
  )

  cat(
    sprintf(
      "\n\nThere are %d unique entries for ICD-10 codes, of which %d (%.2f%%)",
      summary$final_unique_icds,
      summary$final_direct_matches,
      (summary$final_direct_matches /
        summary$final_unique_icds) * 100
    ), "are directly in the Thai ICD-10 library.\n"
  )

  cat(
    sprintf(
      "The modifications led to a total of %d codes",
      summary$final_unique_icds -
        summary$final_unmatched
    ), "being mapped to an equivalent in the Thai ICD10 library.\n"
  )

  cat(
    sprintf(
      "Out of these, %d were modified to match.\n",
      summary$final_unique_icds -
        summary$final_unmatched -
        summary$final_direct_matches
    )
  )

  cat(
    sprintf(
      "There are %d codes that could not",
      summary$final_unmatched
    ), "be mapped to the Thai ICD10 library.\n"
  )

  unique_icd10_map <- process_final_icd10_map(summary$final_icd10_map_dt, tmp_nrow)

  # Count the number of rows with phl_icd10 length > 5
  num_long_phl_icd10 <- sum(nchar(unique_icd10_map$phl_icd10) > 5)

  # Print the count
  cat(
    "\nNumber of rows with phl_icd10 length greater than 5:",
    num_long_phl_icd10, "of", nrow(unique_icd10_map), "rows"
  )

  # Check if there are any rows and print the table
  if (nrow(unique_icd10_map) > 0) {
    print(
      kable(
        head(
          unique_icd10_map,
          end_nrow
        ),
        format = "markdown",
        caption = "Modified ICD-10 codes ordered by descending NChar distance"
      )
    )
  } else {
    cat("\nNo modified ICD-10 codes found.\n\n")
  }

  if (nrow(summary$final_unmatched_sources) > 0) {
    print(
      kable(
        head(
          summary$final_unmatched_sources,
          end_nrow
        ),
        format = "markdown",
        caption = "Invalid ICD-10 Codes Not Found in Thai Library"
      )
    )
  } else {
    cat("\nAll resulting ICD-10 codes are present in the Thai library.\n\n")
  }

  cat(
    "\nAll PDx's are in list of acceptable PDx's:\n",
    summary$final_rename_success, ""
  )
}

combine_comparison_tables <- function(
    summaries, comparison_field, tmp_nrow = 10) {
  #' @title Combine Comparison Tables
  #'
  #' @description This function combines comparison tables from
  #' multiple summaries into one.
  #'
  #' @param summaries list. A list of summary tables.
  #' @param comparison_field character. The field in the summaries to compare.
  #' @param tmp_nrow integer. The number of
  #' rows to show in the intermediate summary.
  #'
  #' @return data.table. The combined comparison table with absolute differences in character lengths.

  comparison_list <- lapply(summaries, function(summary) {
    summary_data <- summary[[comparison_field]]
    if (!is.null(summary_data) && nrow(summary_data) > 0) {
      summary_data <- summary_data[, .(old_code, new_code)]
    }
    return(summary_data)
  })

  # Combine all the data
  combined_comparison <- rbindlist(comparison_list, fill = TRUE)

  if (nrow(combined_comparison) == 0) {
    return(data.table(
      old_code = character(),
      new_code = character(),
      diff_chars = integer()
    ))
  }

  # Trim whitespaces and calculate the absolute difference in character lengths for unique pairs
  combined_comparison[, `:=`(
    old_code = gsub("\\s", "", iconv(old_code, to = "UTF-8")),
    new_code = gsub("\\s", "", iconv(new_code, to = "UTF-8"))
  )]
  combined_comparison[, diff_chars := abs(nchar(old_code) - nchar(new_code))]

  # Keep only unique old_code to new_code combinations
  unique_combinations <- unique(combined_comparison)

  # Order by the absolute character difference and limit the number of rows
  unique_combinations <- unique_combinations[order(-diff_chars)]
  unique_combinations <- head(unique_combinations, tmp_nrow)

  return(unique_combinations)
}

process_final_icd10_map <- function(icd10_map_dt, tmp_nrow = 10) {
  #' @title Process Final ICD-10 Map
  #'
  #' @description This function processes the final ICD-10 map data table
  #' by filtering out rows where the original and mapped codes are the same
  #' and sorts the result by descending absolute difference in the number
  #' of characters between the codes.
  #'
  #' @param icd10_map_dt data.table. The ICD-10 map data table.
  #' @param tmp_nrow integer. The number of rows to show in the final summary.
  #'
  #' @return data.table. The processed and sorted ICD-10 map.

  # Filter rows where phl_icd10 and tdrg_icd10 are different
  icd10_map_dt <- icd10_map_dt[phl_icd10 != tdrg_icd10]

  if (nrow(icd10_map_dt) == 0) {
    return(data.table(
      phl_icd10 = character(),
      tdrg_icd10 = character(),
      char_diff = numeric()
    ))
  }

  # Calculate the absolute difference in character length and add char_diff column
  icd10_map_dt[, char_diff := abs(nchar(phl_icd10) - nchar(tdrg_icd10))]

  # Sort by descending absolute difference in character length
  icd10_map_dt <- icd10_map_dt[order(-char_diff)]

  # Select the top rows based on tmp_nrow
  final_icd10_map <- head(icd10_map_dt, tmp_nrow)

  return(final_icd10_map)
}

combine_discarded_rvs_tables <- function(
    summaries, field, tmp_nrow = 10) {
  #' @title Combine Discarded RVS Tables
  #'
  #' @description This function combines discarded RVS tables
  #' from multiple summaries into one.
  #'
  #' @param summaries list. A list of summary tables.
  #' @param field character. The field in the summaries that
  #' contains discarded RVS codes.
  #' @param tmp_nrow integer. The number of
  #' rows to show in the intermediate summary.
  #'
  #' @return data.table. The combined discarded RVS table.

  discarded_list <- lapply(summaries, function(summary) summary[[field]])
  combined_discarded <- rbindlist(discarded_list, fill = TRUE)

  if (nrow(combined_discarded) == 0) {
    return(data.table(CODE = character(), count = integer()))
  }

  combined_discarded <- combined_discarded[, .(count = sum(count)), by = CODE]
  combined_discarded <- combined_discarded[order(-count)]
  combined_discarded <- head(combined_discarded, tmp_nrow)

  return(combined_discarded)
}

combine_unmatched_icd10_codes <- function(
    summaries, field, tmp_nrow = 10) {
  #' @title Combine Unmatched ICD-10 Codes
  #'
  #' @description This function combines unmatched ICD-10 codes
  #' from multiple summaries into one.
  #'
  #' @param summaries list. A list of summary tables.
  #' @param field character. The field in the summaries that
  #' contains unmatched ICD-10 codes.
  #' @param tmp_nrow integer. The number of
  #' rows to show in the intermediate summary.
  #'
  #' @return data.table. The combined unmatched ICD-10 codes table.

  combined_list <- lapply(summaries, function(summary) summary[[field]])
  combined_table <- rbindlist(combined_list, fill = TRUE)
  combined_table <- combined_table[, .(count = sum(count)),
    by = .(code, source)
  ]
  combined_table <- combined_table[order(-count)]
  return(combined_table)
}

combine_replace_empty_tables <- function(
    summaries, field, tmp_nrow = 10) {
  #' @title Combine Replace Empty Tables
  #'
  #' @description This function combines tables for replaced
  #' empty values from multiple summaries into one.
  #'
  #' @param summaries list. A list of summary tables.
  #' @param field character. The field in the summaries that
  #' contains information on replaced empty values.
  #' @param tmp_nrow integer. The number of
  #' rows to show in the intermediate summary.
  #'
  #' @return data.table. The combined replace empty tables.

  replace_empty_list <- lapply(summaries, function(summary) summary[[field]])
  combined_replace_empty <- rbindlist(replace_empty_list, fill = TRUE)

  if (nrow(combined_replace_empty) == 0) {
    return(data.table(
      Column = character(),
      Empty_Replaced = integer(),
      NA_Replaced = integer(),
      Character0_Replaced = integer()
    ))
  }

  combined_replace_empty <- combined_replace_empty[, .(
    Empty_Replaced = sum(Empty_Replaced, na.rm = TRUE),
    NA_Replaced = sum(NA_Replaced, na.rm = TRUE),
    Character0_Replaced = sum(Character0_Replaced, na.rm = TRUE)
  ), by = Column]
  combined_replace_empty <- combined_replace_empty[
    order(-Empty_Replaced, -NA_Replaced, -Character0_Replaced)
  ]
  combined_replace_empty <- head(
    combined_replace_empty,
    tmp_nrow
  )

  return(combined_replace_empty)
}

final_combine_replace_empty_tables <- function(
    summaries, field, tmp_nrow = 10) {
  #' @title Combine Replace Empty Tables
  #'
  #' @description This function combines tables for replaced
  #' empty values from multiple summaries into one.
  #'
  #' @param summaries list. A list of summary tables.
  #' @param field character. The field in the summaries that
  #' contains information on replaced empty values.
  #' @param tmp_nrow integer. The number of
  #' rows to show in the intermediate summary.
  #'
  #' @return data.table. The combined replace empty tables.

  replace_empty_list <- lapply(summaries, function(summary) summary[[field]])
  combined_replace_empty <- rbindlist(replace_empty_list, fill = TRUE)

  if (nrow(combined_replace_empty) == 0) {
    return(data.table(
      Column = character(),
      Total_Empty_Replaced = integer(),
      Total_NA_Replaced = integer(),
      Total_Character0_Replaced = integer(),
      Total_Elements = integer(),
      Empty_Replaced_Percentage = character(),
      NA_Replaced_Percentage = character(),
      Character0_Replaced_Percentage = character()
    ))
  }

  combined_replace_empty <- combined_replace_empty[, .(
    Total_Empty_Replaced = sum(Empty_Replaced, na.rm = TRUE),
    Total_NA_Replaced = sum(NA_Replaced, na.rm = TRUE),
    Total_Character0_Replaced = sum(Character0_Replaced, na.rm = TRUE),
    Total_Elements = if (to_sample) sample_size * split_parts else total_rows
  ), by = Column]

  combined_replace_empty[, `:=`(
    Empty_Replaced_Percentage = (Total_Empty_Replaced / Total_Elements) * 100,
    NA_Replaced_Percentage = (Total_NA_Replaced / Total_Elements) * 100,
    Character0_Replaced_Percentage = (Total_Character0_Replaced / Total_Elements) * 100
  )]

  # Format percentages as "XX.X%"
  combined_replace_empty[, `:=`(
    Empty_Replaced_Percentage = sprintf("%.2f%%", Empty_Replaced_Percentage),
    NA_Replaced_Percentage = sprintf("%.2f%%", NA_Replaced_Percentage),
    Character0_Replaced_Percentage = sprintf("%.2f%%", Character0_Replaced_Percentage)
  )]

  combined_replace_empty <- combined_replace_empty[
    order(
      -as.numeric(gsub("%", "", Empty_Replaced_Percentage)),
      -as.numeric(gsub("%", "", NA_Replaced_Percentage)),
      -as.numeric(gsub("%", "", Character0_Replaced_Percentage))
    )
  ]

  combined_replace_empty <- head(combined_replace_empty, tmp_nrow)

  return(combined_replace_empty[, .(
    Column,
    Empty_Replaced_Percentage,
    NA_Replaced_Percentage,
    Character0_Replaced_Percentage
  )])
}


combine_chunk_summaries <- function(
    summaries, tmp_nrow) {
  #' @title Combine Chunk Summaries
  #'
  #' @description This function combines summaries from
  #' multiple chunks into one summary.
  #'
  #' @param summaries list. A list of results from
  #' summaries from parallel processing.
  #' @param tmp_nrow integer. The number
  #' of rows to show in the intermediate summary.
  #'
  #' @return list. The combined summary.

  combined_summary <- combine_summaries(summaries, tmp_nrow)
  return(combined_summary)
}

combine_summaries <- function(summaries, tmp_nrow) {
  #' @title Combine Summaries
  #'
  #' @description This function combines multiple summaries into one summary.
  #'
  #' @param summaries list. A list of summary tables.
  #' @param tmp_nrow integer.
  #' The number of rows to show in the intermediate summary.
  #'
  #' @return list. The combined summary.

  combined_summary <- list(
    rename_success = all(unlist(sapply(
      summaries,
      function(summary) summary$rename_success
    )), na.rm = TRUE),
    ICD_replacements_1 = combine_comparison_tables(
      summaries, "ICD_replacements_1", tmp_nrow
    ),
    ICD_replacements_2 = combine_comparison_tables(
      summaries, "ICD_replacements_2", tmp_nrow
    ),
    pat_type_unmapped = unique(unlist(lapply(
      summaries,
      function(summary) summary$pat_type_unmapped
    ))),
    memcat_parent_unmapped = unique(unlist(lapply(
      summaries,
      function(summary) summary$memcat_parent_unmapped
    ))),
    memcat_child_unmapped = unique(unlist(lapply(
      summaries,
      function(summary) summary$memcat_child_unmapped
    ))),
    discharge_unmapped = unique(unlist(lapply(
      summaries,
      function(summary) summary$discharge_unmapped
    ))),
    claim_status_unmapped = unique(unlist(lapply(
      summaries,
      function(summary) summary$claim_status_unmapped
    ))),
    discard_rvs_one = combine_discarded_rvs_tables(
      summaries, "discard_rvs_one", tmp_nrow
    ),
    discard_rvs_two = combine_discarded_rvs_tables(
      summaries, "discard_rvs_two", tmp_nrow
    ),
    empty_strings_replaced_1 = combine_replace_empty_tables(
      summaries, "empty_strings_replaced_1", tmp_nrow
    ),
    empty_strings_replaced_2 = combine_replace_empty_tables(
      summaries, "empty_strings_replaced_2", tmp_nrow
    ),
    unique_icds_count = unique(unlist(lapply(
      summaries,
      function(summary) summary$unique_icds
    ))),
    direct_matches_count = unique(unlist(lapply(
      summaries,
      function(summary) summary$direct_matches
    ))),
    unmatched_count = unique(unlist(lapply(
      summaries,
      function(summary) summary$unmatched
    ))),
    unmatched_sources = combine_unmatched_icd10_codes(
      summaries, "unmatched_sources", tmp_nrow
    ),
    rvss = unique(na.omit(unlist(lapply(
      summaries,
      function(summary) summary$rvss
    )))),
    mappable_rvs = unique(na.omit(unlist(lapply(
      summaries,
      function(summary) summary$mappable_rvs
    )))),
    unmappable_rvs = unique(na.omit(unlist(lapply(
      summaries,
      function(summary) summary$unmappable_rvs
    )))),
    multi_mapped_rvs = unique(na.omit(unlist(lapply(
      summaries,
      function(summary) summary$multi_mapped_rvs
    )))),
    without_drg = unique(na.omit(unlist(lapply(
      summaries,
      function(summary) summary$without_drg
    )))),
    pdx_success = all(unlist(sapply(
      summaries,
      function(summary) summary$pdx_success
    )), na.rm = TRUE),
    icd10_map_dt = rbindlist(lapply(
      summaries,
      function(summary) summary$icd10_map_dt
    )),
    pat_type_mapped = rbindlist(lapply(
      summaries,
      function(summary) summary$pat_type_mapped
    )),
    pat_memcat_parent_mapped = rbindlist(lapply(
      summaries,
      function(summary) summary$pat_memcat_parent_mapped
    )),
    pat_memcat_child_mapped = rbindlist(lapply(
      summaries,
      function(summary) summary$pat_memcat_child_mapped
    )),
    clin_discharge_mapped = rbindlist(lapply(
      summaries,
      function(summary) summary$clin_discharge_mapped
    )),
    claim_status_mapped = rbindlist(lapply(
      summaries,
      function(summary) summary$claim_status_mapped
    ))
  )

  return(combined_summary)
}

combine_parts_summaries <- function(combined_summary, end_nrow) {
  #' @title Combine Parts Summaries
  #'
  #' @description This function combines summaries from multiple
  #' parts into one final summary.
  #'
  #' @param combined_summary list. A list of combined summaries.
  #' @param end_nrow integer. The number of rows to show in
  #' the final summary.
  #'
  #' @return list. The final combined summary.

  final_combined_summaries <- list(
    final_rename_success = all(unlist(sapply(
      combined_summary,
      function(summary) summary$rename_success
    )), na.rm = TRUE),
    final_ICD_replacements_1 = combine_comparison_tables(
      combined_summary, "ICD_replacements_1", end_nrow
    ),
    final_ICD_replacements_2 = combine_comparison_tables(
      combined_summary, "ICD_replacements_2", end_nrow
    ),
    final_pat_type_unmapped = unique(unlist(lapply(
      combined_summary,
      function(summary) summary$pat_type_unmapped
    ))),
    final_memcat_parent_unmapped = unique(unlist(lapply(
      combined_summary,
      function(summary) summary$memcat_parent_unmapped
    ))),
    final_memcat_child_unmapped = unique(unlist(lapply(
      combined_summary,
      function(summary) summary$memcat_child_unmapped
    ))),
    final_discharge_unmapped = unique(unlist(lapply(
      combined_summary,
      function(summary) summary$discharge_unmapped
    ))),
    final_claim_status_unmapped = unique(unlist(lapply(
      combined_summary,
      function(summary) summary$claim_status_unmapped
    ))),
    final_discard_rvs_one = combine_discarded_rvs_tables(
      combined_summary, "discard_rvs_one", tmp_nrow
    ),
    final_discard_rvs_two = combine_discarded_rvs_tables(
      combined_summary, "discard_rvs_two", tmp_nrow
    ),
    final_empty_strings_replaced_0 = final_combine_replace_empty_tables(
      combined_summary, "replacement_summary", end_nrow
    ),
    final_empty_strings_replaced_1 = final_combine_replace_empty_tables(
      combined_summary, "empty_strings_replaced_1", end_nrow
    ),
    final_empty_strings_replaced_2 = final_combine_replace_empty_tables(
      combined_summary, "empty_strings_replaced_2", end_nrow
    ),
    final_unique_icds = length(unique(unlist(lapply(
      combined_summary,
      function(summary) summary$unique_icds_count
    )))),
    final_direct_matches = length(unique(unlist(lapply(
      combined_summary,
      function(summary) summary$direct_matches_count
    )))),
    final_unmatched = length(unique(unlist(lapply(
      combined_summary,
      function(summary) summary$unmatched_count
    )))),
    final_unmatched_sources = combine_unmatched_icd10_codes(
      combined_summary, "unmatched_sources", end_nrow
    ),
    final_rvss = length(unique(na.omit(unlist(lapply(
      combined_summary,
      function(summary) summary$rvss
    ))))),
    final_mappable_rvs = length(unique(na.omit(unlist(lapply(
      combined_summary,
      function(summary) summary$mappable_rvs
    ))))),
    final_unmappable_rvs = length(unique(na.omit(unlist(lapply(
      combined_summary,
      function(summary) summary$unmappable_rvs
    ))))),
    final_multi_mapped_rvs = length(unique(na.omit(unlist(lapply(
      combined_summary,
      function(summary) summary$multi_mapped_rvs
    ))))),
    final_without_drg = length(unique(na.omit(unlist(lapply(
      combined_summary,
      function(summary) summary$without_drg
    ))))),
    final_pdx_success = all(unlist(sapply(
      combined_summary,
      function(summary) summary$pdx_success
    )), na.rm = TRUE),
    final_icd10_map_dt = unique(rbindlist(lapply(
      combined_summary,
      function(summary) summary$icd10_map_dt
    ))),
    final_pat_type_mapped = unique(rbindlist(lapply(
      combined_summary,
      function(summary) summary$pat_type_mapped
    ))),
    final_memcat_parent_mapped = unique(rbindlist(lapply(
      combined_summary,
      function(summary) summary$pat_memcat_parent_mapped
    ))),
    final_memcat_child_mapped = unique(rbindlist(lapply(
      combined_summary,
      function(summary) summary$pat_memcat_child_mapped
    ))),
    final_clin_discharge_mapped = unique(rbindlist(lapply(
      combined_summary,
      function(summary) summary$clin_discharge_mapped
    ))),
    final_claim_status_mapped = unique(rbindlist(lapply(
      combined_summary,
      function(summary) summary$claim_status_mapped
    )))
  )

  return(final_combined_summaries)
}
ensure_sample_files_exist <- function(sample_part) {
  #' @title Ensure Sample Files Exist
  #' @description This function checks if sample files exist for a given sample_part and creates them if they don't.
  #' @param sample_part integer. The sample_part number to process.
  #' @return NULL. Creates sample files as a side effect if they do not exist.
  #'
  set.seed(global_seed)
  if (!file.exists(sampled_claims_file)) {
    dt <- readRDS(
      here(raw_claims_parts_path, paste0(
        full_claims_prefix, year_to_load,
        "_part_", sprintf("%02d", sample_part), "_of_", split_parts, ".rds"
      ))
    )
    dt <- dt[sample(.N, min(sample_size, .N))]
    # setnames(dt, colnames(full_header))
    saveRDS(dt, sampled_claims_file, compress = FALSE)
  }
}

read_appropriate_file <- function(read_part, to_sample) {
  #' @title Read Appropriate File
  #' @description This function reads the appropriate file (partial or sample)
  #' for a given read_part, drops specified columns, and casts column types.
  #' @param read_part integer. The read_part number to process.
  #' @param to_sample logical. Whether to read the sample file or the
  #' full partial file.
  #' @return data.table. The processed data table.

  chunk_file <- if (to_sample) {
    sampled_claims_file
  } else {
    here(raw_claims_parts_path, paste0(
      full_claims_prefix, year_to_load,
      "_part_", sprintf("%02d", read_part), "_of_", split_parts, ".rds"
    ))
  }

  dt <- readRDS(chunk_file)

  available_columns <- colnames(dt)

  if (to_debug) print(head(dt), 2) # debug

  # Drop columns
  if (any(drop_cols %in% available_columns)) {
    dt <- dt[, (drop_cols) := NULL]
  }

  if (any(drop_cols_manual %in% available_columns)) {
    dt <- dt[, (drop_cols_manual) := NULL]
  }


  replace_result <- replace_empty_with_na(dt = dt, to_view_checks)
  dt <- replace_result$return_data
  replacement_summary <- replace_result$return_replacement_summary

  if (to_debug) print(head(dt), 2) # debug

  ## Apply column classes only to the columns that exist in the data
  col_classes <- sapply(available_columns, function(col) {
    if (col %in% unlist(expected_types["character"])) {
      return("character")
    }
    if (col %in% unlist(expected_types["integer"])) {
      return("integer")
    }
    if (col %in% unlist(expected_types["factor"])) {
      return("factor")
    }
    if (col %in% unlist(expected_types["numeric"])) {
      return("numeric")
    }
  })

  if (to_debug) print(col_classes)

  # Cast column types with checks
  for (col in names(col_classes)) {
    original_values <- dt[[col]]

    dt[[col]] <- switch(col_classes[[col]],
      "character" = as.character(dt[[col]]),
      "factor" = {
        levels <- unique(dt[[col]])
        as.factor(dt[[col]])
      },
      "integer" = {
        suppressWarnings(as.integer(dt[[col]]))
      },
      "numeric" = {
        suppressWarnings(as.numeric(dt[[col]]))
      },
      dt[[col]] # Default case: no conversion if unrecognized type
    )

    # Check for NA coercion
    coerced_to_na <- which(is.na(dt[[col]]) & !is.na(original_values))
    if (length(coerced_to_na) > 0) {
      cat(sprintf(
        "Column '%s' coerced %d values to NA. First few original values: %s\n",
        col, length(coerced_to_na), paste(original_values[coerced_to_na][1:5],
          collapse = ", "
        )
      ))
    }
  }

  nrow_start[[read_part]] <<- nrow(dt)

  if (to_debug) print(paste0("Available Columns: ", available_columns))

  # # Inspect a few rows before and after conversion
  if (to_debug) print(head(dt$ADMISSION_TIME))
  if (to_debug) print(head(dt$DISCHARGE_TIME))

  return(
    list(
      read_result_dt = dt,
      read_result_replacement_summary = replacement_summary
    )
  )
}

export_for_grouper <- function(dt, output_txt_file) {
  #' @title Export Data for Batch Grouper
  #'
  #' @description This function exports data for batch grouper,
  #' generating necessary columns and formatting them accordingly.
  #'
  #' @param dt data.table. The input data table.
  #' @param output_txt_file character. The path to the output text file.
  #'
  #' @return NULL.

  output_dt_thai <- data.table()

  # CASEID
  output_dt_thai[, CASEID := dt$id_series]

  # Format Date of Birth (DOB) and Age
  output_dt_thai[, DOB := format(dt$pat_bdate, "%d/%m/%Y")]

  # Format Sex
  output_dt_thai[, Sex := ifelse(dt$pat_sex == "M", 1, 2)]

  # Format Admission Date and Time
  output_dt_thai[, DateAdm := format(ymd(dt$date_adm), "%d/%m/%Y")]
  output_dt_thai[, TimeAdm := format(as.POSIXct(dt$time_adm, format = "%H:%M:%S"), "%H%M")]

  # Format Discharge Date and Time
  output_dt_thai[, DateDsc := format(ymd(dt$date_dis), "%d/%m/%Y")]
  output_dt_thai[, TimeDsc := format(as.POSIXct(dt$time_dis, format = "%H:%M:%S"), "%H%M")]

  # Discharge Type
  output_dt_thai[, DischT := dt$clin_discharge]

  # Admission Weight
  output_dt_thai[, AdmWt := dt$pat_bwt]

  # Principal Diagnosis Code
  output_dt_thai[, PDx := dt$clin_pdx]

  # Secondary Diagnosis Codes (SDx1 to SDx12)
  icd_codes_list <- lapply(dt$clin_sdx, function(icd_str) {
    codes <- unlist(icd_str)
    length(codes) <- 12 # Ensure there are 12 elements
    codes
  })
  icd_codes <- as.data.table(do.call(rbind, icd_codes_list))
  icd_cols <- paste0("SDx", 1:12)
  output_dt_thai[, (icd_cols) := icd_codes]

  # Procedure Codes (Proc1 to Proc20)
  proc_codes_list <- lapply(dt$clin_proc, function(proc_str) {
    codes <- unlist(proc_str)
    length(codes) <- 20 # Ensure there are 20 elements
    codes
  })
  proc_codes <- as.data.table(do.call(rbind, proc_codes_list))
  proc_cols <- paste0("Proc", 1:20)
  output_dt_thai[, (proc_cols) := proc_codes]

  # Replace NA values with '--'
  output_dt_thai[is.na(output_dt_thai)] <- "--"

  str(output_dt_thai)

  # Write the data.table to a file with vertical bar (|) as delimiter
  fwrite(output_dt_thai, output_txt_file, sep = "|", col.names = TRUE)

  if (to_debug) {
    return(NULL)
  }
}
