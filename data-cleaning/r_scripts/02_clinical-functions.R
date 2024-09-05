remap_patient_type <- function(pat_type) {
  #' @title Remap patient type
  #'
  #' @description This function remaps patient types to standardized codes
  #' and identifies any unknown types.
  #'
  #' @param pat_type character. The patient type column.
  #'
  #' @return list. A list containing the remapped patient types
  #' and the unknown types.
  known_types <- c("MEMBER", "DEPENDENT")
  remapped_pat_type <- fcase(
    pat_type == "MEMBER", "M",
    pat_type == "DEPENDENT", "D"
  )
  unknown_types <- setdiff(
    pat_type[!is.na(pat_type)],
    known_types
  )

  # Check for unmapped types and print a warning
  if (length(unknown_types) > 0) {
    warning(sprintf(
      "Unmapped Patient Types: %s",
      paste(unknown_types, collapse = ", ")
    ))
    # cat("Unmapped Patient Types:\n")
    # print(unknown_types)
  }

  list(
    # main return variable to be saved back to dt
    remapped = remapped_pat_type,
    # other return variables for checks and outputs
    original = pat_type,
    unmapped = unknown_types
  )
}

remap_claim_status <- function(claim_status) {
  #' @title Remap claim status
  #'
  #' @description This function remaps claim statuses to standardized codes
  #' and identifies any unknown statuses.
  #'
  #' @param claim_status character. The claim status column.
  #'
  #' @return list. A list containing the remapped claim statuses
  #' and the unknown statuses.
  known_types <- c("DENIED", "IN-PROCESS", "PAID", "RTH", "APRV4PAYMENT")
  remapped_claim_status <- fcase(
    claim_status == "DENIED", "D",
    claim_status == "IN-PROCESS", "I",
    claim_status == "PAID", "G",
    claim_status == "RTH", "R",
    claim_status == "APRV4PAYMENT", "G"
  )
  unknown_types <- setdiff(
    claim_status[!is.na(claim_status)],
    known_types
  )

  # Check for unmapped claim statuses and print a warning
  if (length(unknown_types) > 0) {
    warning(sprintf(
      "Unmapped Claim Statuses: %s",
      paste(unknown_types, collapse = ", ")
    ))
    # cat("Unmapped Claim Statuses:\n")
    # print(unknown_types)
  }

  list(
    # main return variable to be saved back to dt
    remapped = remapped_claim_status,
    # other return variables for checks and outputs
    original = claim_status,
    unmapped = unknown_types
  )
}

remap_memcat_parent_desc <- function(pat_memcat_parent) {
  #' @title Remap member category parent description
  #'
  #' @description This function remaps member category parent descriptions
  #' to standardized codes and identifies any unknown parents.
  #'
  #' @param pat_memcat_parent character. The member category parent
  #' description column.
  #'
  #' @return list. A list containing the remapped parent descriptions
  #' and the unknown parents.
  known_parents <- c("DIRECT CONTRIBUTOR", "INDIRECT CONTRIBUTOR")
  remapped_memcat_parent <- fcase(
    pat_memcat_parent == "DIRECT CONTRIBUTOR", "D",
    pat_memcat_parent == "INDIRECT CONTRIBUTOR", "I"
  )
  unknown_parents <- setdiff(
    pat_memcat_parent[!is.na(pat_memcat_parent)],
    known_parents
  )

  # Check for unmapped parent descriptions and print a warning
  if (length(unknown_parents) > 0) {
    warning(sprintf(
      "Unmapped Memcat Parent Descriptions: %s",
      paste(unknown_parents, collapse = ", ")
    ))
    # cat("Unmapped Memcat Parent Descriptions:\n")
    # print(unknown_parents)
  }

  list(
    # main return variable to be saved back to dt
    remapped = remapped_memcat_parent,
    # other return variables for checks and outputs
    original = pat_memcat_parent,
    unmapped = unknown_parents
  )
}

remap_memcat_child_desc <- function(pat_memcat_child) {
  #' @title Remap member category child description
  #'
  #' @description This function remaps member category child descriptions
  #' to standardized codes and identifies any unknown children.
  #'
  #' @param pat_memcat_child character. The member category child
  #' description column.
  #'
  #' @return list. A list containing the remapped child descriptions
  #' and the unknown children.
  known_children <- c(
    "EMPLOYED PRIVATE", "SELF-EARNING INDIVIDUAL", "SENIOR CITIZEN", "INDIGENT",
    "LIFETIME MEMBER", "SPONSORED", "MIGRANT WORKER", "EMPLOYED GOVERNMENT",
    "INFORMAL ECONOMY", "HOUSEHOLD HELP/KASAMBAHAY", "FOREIGN NATIONAL",
    "FILIPINOS WITH DUAL CITIZENSHIP / LIVING ABROAD",
    "SELF EARNING INDIVIDUAL", "FAMILY DRIVER", "FORMAL ECONOMY",
    "PROFESSIONAL PRACTITIONER", "DIRECT CONTRIBUTOR"
  )
  remapped_memcat_child <- fcase(
    pat_memcat_child == "EMPLOYED PRIVATE", "FORMAL",
    pat_memcat_child == "SELF-EARNING INDIVIDUAL", "INFORMAL",
    pat_memcat_child == "SENIOR CITIZEN", "SENIOR",
    pat_memcat_child == "INDIGENT", "INDIGENT",
    pat_memcat_child == "LIFETIME MEMBER", "LIFETIME",
    pat_memcat_child == "SPONSORED", "SPONSORED",
    pat_memcat_child == "MIGRANT WORKER", "INFORMAL",
    pat_memcat_child == "EMPLOYED GOVERNMENT", "FORMAL",
    pat_memcat_child == "INFORMAL ECONOMY", "INFORMAL",
    pat_memcat_child == "HOUSEHOLD HELP/KASAMBAHAY", "FORMAL",
    pat_memcat_child == "FOREIGN NATIONAL", "INFORMAL",
    pat_memcat_child == "FILIPINOS WITH DUAL CITIZENSHIP / LIVING ABROAD",
    "INFORMAL",
    pat_memcat_child == "SELF EARNING INDIVIDUAL", "INFORMAL",
    pat_memcat_child == "FAMILY DRIVER", "FORMAL",
    # added this myself
    pat_memcat_child == "FORMAL ECONOMY", "FORMAL",
    # added this myself
    pat_memcat_child == "DIRECT CONTRIBUTOR", "FORMAL",
    # added this myself
    pat_memcat_child == "PROFESSIONAL PRACTITIONER", "INFORMAL"
  )
  unknown_children <- setdiff(
    pat_memcat_child[!is.na(pat_memcat_child)],
    known_children
  )

  # Check for unmapped child descriptions and print a warning
  if (length(unknown_children) > 0) {
    warning(sprintf(
      "Unmapped Memcat Child Descriptions: %s",
      paste(unknown_children, collapse = ", ")
    ))
    # cat("Unmapped Memcat Child Descriptions:\n")
    # print(unknown_children)
  }

  list(
    # main return variable to be saved back to dt
    remapped = remapped_memcat_child,
    # other return variables for checks and outputs
    original = pat_memcat_child,
    unmapped = unknown_children
  )
}

remap_disposition <- function(clin_discharge) {
  #' @title Remap clinical discharge disposition
  #'
  #' @description This function remaps clinical discharge dispositions to
  #' standardized codes and identifies any unknown dispositions.
  #'
  #' @param clin_discharge character. The clinical discharge disposition column.
  #'
  #' @return list. A list containing the remapped discharge dispositions
  #' and the unknown dispositions.
  known_dispositions <- c(
    "IMPROVED", "RECOVERED", "HOME/DISCHARGED AGAINST MEDICAL ADVICE",
    "ABSCONDED", "TRANSFERRED/REFERRED", "EXPIRED", "UNDEFINED"
  )
  remapped_discharge <- fcase(
    clin_discharge == "IMPROVED", 1L,
    clin_discharge == "RECOVERED", 1L,
    clin_discharge == "HOME/DISCHARGED AGAINST MEDICAL ADVICE", 2L,
    clin_discharge == "ABSCONDED", 3L,
    clin_discharge == "TRANSFERRED/REFERRED", 4L,
    clin_discharge == "EXPIRED", 9L,
    clin_discharge == "UNDEFINED", NA_integer_
  )
  unknown_dispositions <- setdiff(
    clin_discharge[!is.na(clin_discharge)],
    known_dispositions
  )

  # Check for unmapped discharge dispositions and print a warning
  if (length(unknown_dispositions) > 0) {
    warning(sprintf(
      "Unmapped Discharge Dispositions: %s",
      paste(unknown_dispositions, collapse = ", ")
    ))
    # cat("Unmapped Discharge Dispositions:\n")
    # print(unknown_dispositions)
  }

  list(
    # main return variable to be saved back to dt
    remapped = remapped_discharge,
    # other return variables for checks and outputs
    original = clin_discharge,
    unmapped = unknown_dispositions
  )
}

remap_patient_data <- function(dt, to_view_checks) {
  #' @title Remap patient data
  #'
  #' @description This function remaps patient data such as
  #' patient type, member category, and discharge disposition
  #' in the data.table.
  #'
  #' @param dt data.table. The data table to be processed.
  #' @param to_view_checks logical. Whether to view checks.
  #'
  #' @return list. A list containing the processed data and summaries.

  # Load necessary library
  library(data.table)

  # Initialize lists for unmapped variables
  pat_unmap <- NULL
  parent_unmap <- NULL
  child_unmap <- NULL
  discharge_unmap <- NULL
  claim_status_unmap <- NULL

  # Initialize data tables for mapped variables
  pat_mapped <- data.table(
    Original = character(), Mapped = character()
  )
  parent_mapped <- data.table(
    Original = character(), Mapped = character()
  )
  child_mapped <- data.table(
    Original = character(), Mapped = character()
  )
  discharge_mapped <- data.table(
    Original = character(), Mapped = character()
  )
  claim_status_mapped <- data.table(
    Original = character(), Mapped = character()
  )

  # Remap patient type
  result <- remap_patient_type(dt$pat_type)
  dt$pat_type <- result$remapped

  # Create a data table for mapped patient types
  pat_mapped <- unique(
    data.table(Original = result$original, Mapped = result$remapped)
  )

  # Capture unmapped patient types if needed
  if (length(result$unmapped) > 0 && to_view_checks) {
    pat_unmap <- result$unmapped
  }

  # Remap member category parent
  result <- remap_memcat_parent_desc(dt$pat_memcat_parent)
  dt$pat_memcat_parent <- result$remapped

  # Create a data table for mapped member category parents
  parent_mapped <- unique(
    data.table(Original = result$original, Mapped = result$remapped)
  )

  # Capture unmapped member category parents if needed
  if (length(result$unmapped) > 0 && to_view_checks) {
    parent_unmap <- result$unmapped
  }

  # Remap member category child
  result <- remap_memcat_child_desc(dt$pat_memcat_child)
  dt$pat_memcat_child <- result$remapped

  # Create a data table for mapped member category children
  child_mapped <- unique(
    data.table(Original = result$original, Mapped = result$remapped)
  )

  # Capture unmapped member category children if needed
  if (length(result$unmapped) > 0 && to_view_checks) {
    child_unmap <- result$unmapped
  }

  # Remap discharge disposition
  result <- remap_disposition(dt$clin_discharge)
  dt$clin_discharge <- result$remapped

  # Create a data table for mapped discharge dispositions
  discharge_mapped <- unique(
    data.table(Original = result$original, Mapped = result$remapped)
  )

  # Capture unmapped discharge dispositions if needed
  if (length(result$unmapped) > 0 && to_view_checks) {
    discharge_unmap <- result$unmapped
  }

  # Remap claim status
  result <- remap_claim_status(dt$claim_status)
  dt$claim_status <- result$remapped

  # Create a data table for mapped claim statuses
  claim_status_mapped <- unique(
    data.table(Original = result$original, Mapped = result$remapped)
  )

  # Capture unmapped claim statuses if needed
  if (length(result$unmapped) > 0 && to_view_checks) {
    claim_status_unmap <- result$unmapped
  }

  return(
    list(
      # main data return
      data = dt,
      # other return variables for checks and outputs
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
  #' @title Remove Lumped ICD Codes
  #'
  #' @description This function removes lumped ICD codes by adding a separator
  #' between numeric and alphabetic characters.
  #'
  #' @param column character. The column to be processed.
  #'
  #' @return character. The modified column with lumped ICD codes separated.
  # Use stri_replace_all_regex with a regex pattern for the desired replacement
  modified_column <- stri_replace_all_regex(
    column,
    "(?<=\\d)(?=[A-Za-z])",
    "||",
    opts_regex = stri_opts_regex()
  )
  return(modified_column)
}

# Function to transfer extra ICD-10 codes to clinical ICD
transfer_extra_icd10s_to_clin_icd <- function(clin_icd, col) {
  #' @title Transfer Extra ICD-10 Codes to Clinical ICD
  #'
  #' @description This function transfers extra ICD-10 codes from
  #' a column to the clinical ICD.
  #'
  #' @param clin_icd list. The clinical ICD codes.
  #' @param col list. The column containing extra ICD-10 codes.
  #'
  #' @return list. A list containing updated clinical ICD and the
  #' first code of the column.

  clin_icd <- lapply(clin_icd, function(x) if (is.null(x)) character() else x)
  col_first <- lapply(col, function(x) x[1])

  clin_icd <- mapply(function(icd, c1) {
    c(icd, c1[-1])
  }, clin_icd, col, SIMPLIFY = FALSE)

  return(list(clin_icd = clin_icd, col_first = col_first))
}

# Function to get unique ICD codes
get_unique_icd_codes <- function(clin_c1, clin_c2, clin_icd) {
  #' @title Get Unique ICD Codes
  #'
  #' @description This function retrieves unique ICD codes from
  #' the given columns.
  #'
  #' @param clin_c1 list. The clinical column 1 ICD codes.
  #' @param clin_c2 list. The clinical column 2 ICD codes.
  #' @param clin_icd list. The clinical ICD codes.
  #'
  #' @return character. The unique ICD codes.

  icds <- unique(c(unlist(clin_c1), unlist(clin_c2), unlist(clin_icd)))
  icds <- icds[!is.na(icds)]
  return(icds)
}

# Function to create a Thai ICD-10 environment
create_thai_icd10_environment <- function(thai_icd10_codes) {
  #' @title Create Thai ICD-10 Environment
  #'
  #' @description This function creates an environment for Thai ICD-10 codes.
  #'
  #' @param thai_icd10_codes character. The Thai ICD-10 codes.
  #'
  #' @return environment. The environment with Thai ICD-10 codes.

  thai_icd10_env <- list2env(
    setNames(as.list(rep(TRUE, length(thai_icd10_codes))), thai_icd10_codes)
  )
  return(thai_icd10_env)
}

# Function to find direct ICD matches
find_direct_icd_matches <- function(icds, thai_icd10_env) {
  #' @title Find Direct ICD Matches
  #'
  #' @description This function finds direct matches for ICD codes
  #' in the Thai ICD-10 environment.
  #'
  #' @param icds character. The ICD codes to be matched.
  #' @param thai_icd10_env environment. The environment with Thai ICD-10 codes.
  #'
  #' @return character. The ICD codes that have direct matches.

  direct_matches <- mget(
    icds, thai_icd10_env,
    ifnotfound = as.list(rep(FALSE, length(icds)))
  )
  direct_match_codes <- names(
    unlist(direct_matches[unlist(direct_matches) == TRUE])
  )
  return(direct_match_codes)
}

# Function to generate ICD-10 mapping
generate_icd10_mapping <- function(icds, thai_icd10_env, neoplasms_env) {
  #' @title Generate ICD-10 Mapping
  #'
  #' @description This function generates a mapping of ICD-10 codes
  #' based on the Thai ICD-10 environment.
  #'
  #' @param icds character. The ICD codes to be mapped.
  #' @param thai_icd10_env environment. The environment with Thai ICD-10 codes.
  #' @param neoplasms_env environment. The environment with neoplasm ICD codes.
  #'
  #' @return list. A list containing the ICD mapping and the count of
  #' modified codes.

  icd_mapping <- list()
  modified_count <- 0
  for (d in icds) {
    d <- str_trim(d)
    if (exists(d, thai_icd10_env)) {
      icd_mapping[[d]] <- d
    } else if (
      !exists(d, neoplasms_env) && grepl("[A-Za-z]", d) && grepl("[0-9]", d)) {
      if (nchar(d) == 3 && exists(paste0(d, "9"), thai_icd10_env)) {
        icd_mapping[[d]] <- paste0(d, "9")
        modified_count <- modified_count + 1
      } else if (nchar(d) >= 4) {
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
  return(list(icd_mapping = icd_mapping, modified_count = modified_count))
}

# Helper function to map ICD-10 codes to columns
apply_icd10_mapping_to_columns <- function(
    clin_c1, clin_c2, clin_icd, icd10_env) {
  #' @title Apply ICD-10 Mapping to Columns
  #'
  #' @description This function maps ICD-10 codes to the given columns
  #' using the provided environment.
  #'
  #' @param clin_c1 list. The clinical column 1 ICD codes.
  #' @param clin_c2 list. The clinical column 2 ICD codes.
  #' @param clin_icd list. The clinical ICD codes.
  #' @param icd10_env environment. The environment with ICD-10 codes.
  #'
  #' @return list. A list containing the mapped clinical columns.

  map_icd10_helper <- function(codes) {
    mapped <- mget(codes, icd10_env, ifnotfound = as.list(codes))
    return(unname(unlist(mapped)))
  }

  clin_c1_mapped <- lapply(clin_c1, map_icd10_helper)
  clin_c2_mapped <- lapply(clin_c2, map_icd10_helper)
  clin_icd_mapped <- lapply(clin_icd, map_icd10_helper)

  return(
    list(
      clin_c1 = clin_c1_mapped,
      clin_c2 = clin_c2_mapped,
      clin_icd = clin_icd_mapped
    )
  )
}

# Function to ensure unique ICD codes
ensure_unique_icd_codes <- function(clin_c1, clin_c2, clin_icd) {
  #' @title Ensure Unique ICD Codes
  #'
  #' @description This function ensures that ICD codes are unique
  #' within and across clinical columns.
  #'
  #' @param clin_c1 list. The clinical column 1 ICD codes.
  #' @param clin_c2 list. The clinical column 2 ICD codes.
  #' @param clin_icd list. The clinical ICD codes.
  #'
  #' @return list. A list containing the deduplicated clinical columns.

  # Convert lists to data.table for efficient processing
  datatable <- data.table(
    clin_c1 = clin_c1,
    clin_c2 = clin_c2,
    clin_icd = clin_icd
  )

  # Deduplicate each column
  datatable[, clin_c1 := lapply(clin_c1, unique)]
  datatable[, clin_c2 := lapply(clin_c2, unique)]
  datatable[, clin_icd := lapply(clin_icd, unique)]

  # Remove entries in clin_icd that are in clin_c1 or clin_c2
  datatable[, clin_icd := Map(function(c1, c2, icd) {
    setdiff(icd, union(c1, c2))
  }, clin_c1, clin_c2, clin_icd)]

  # Remove entries in clin_c1 that are in clin_c2
  datatable[, clin_c1 := Map(function(c1, c2) {
    setdiff(c1, c2)
  }, clin_c1, clin_c2)]

  # Remove entries in clin_c2 that are in clin_c1
  datatable[, clin_c2 := Map(function(c1, c2) {
    setdiff(c2, c1)
  }, clin_c1, clin_c2)]

  return(
    list(
      clin_c1 = datatable$clin_c1,
      clin_c2 = datatable$clin_c2,
      clin_icd = datatable$clin_icd
    )
  )
}

split_rvs_codes <- function(rvs_icd9) {
  #' @title Split RVS Codes
  #' @description This function splits RVS codes into those with and without DRG.
  #'
  #' @param rvs_icd9 A data table containing RVS codes and a logical column `is_drg`.
  #'
  #' @return A list containing two data tables: `with_drg` (RVS codes with DRG) and `without_drg` (RVS codes without DRG).

  with_drg <- rvs_icd9[is_drg == TRUE]
  without_drg <- rvs_icd9[!rvs %in% with_drg$rvs]
  return(list(with_drg = with_drg, without_drg = without_drg))
}

create_rvs_map_lists <- function(with_drg) {
  #' @title Create RVS Map Lists
  #' @description This function creates two lists for mapping RVS codes to ICD-9-CM codes: one for solo mappings and one for multi-mappings.
  #'
  #' @param with_drg A data table of RVS codes with corresponding DRG, ordered by `rvs` and `is_drg`.
  #'
  #' @return A list containing `rvs_map_list` (RVS codes with multiple ICD-9-CM mappings) and `rvs_map_solo` (RVS codes with a single ICD-9-CM mapping).

  setorder(with_drg, rvs, -is_drg)
  unique_rvs <- with_drg[, .(icd9cm_list = list(icd9cm)), by = rvs]
  solo <- unique_rvs[lengths(icd9cm_list) == 1]
  list_mapped <- unique_rvs[lengths(icd9cm_list) > 1]

  rvs_map_solo <- setNames(solo$icd9cm_list, solo$rvs)
  rvs_map_list <- setNames(list_mapped$icd9cm_list, list_mapped$rvs)

  return(list(rvs_map_list = rvs_map_list, rvs_map_solo = rvs_map_solo))
}

get_icd9_codes <- function(clin_rvs, rvs_map_solo_env) {
  #' @title Get ICD-9 Codes from Clinical RVS
  #' @description This function retrieves ICD-9 codes based on clinical RVS codes from a given environment.
  #'
  #' @param clin_rvs A list of clinical RVS codes.
  #' @param rvs_map_solo_env An environment containing mappings from RVS codes to ICD-9 codes.
  #'
  #' @return A list of ICD-9 codes corresponding to the provided clinical RVS codes.

  lapply(clin_rvs, function(x) {
    codes <- unlist(x)
    mappable <- codes[
      !is.na(mget(codes, envir = rvs_map_solo_env, ifnotfound = NA))
    ]
    if (length(mappable) > 0) {
      unique(unlist(mget(mappable, envir = rvs_map_solo_env)))
    } else {
      NA_character_
    }
  })
}

find_and_append_valid_rvs <- function(datatable, valid_rvs_codes) {
  #' @title Find and Append Valid RVS Codes
  #' @description This function identifies valid RVS codes in a data table and appends them to existing clinical RVS codes.
  #'
  #' @param datatable A data table containing `clin_rvs` and `col` columns.
  #' @param valid_rvs_codes A vector of valid RVS codes to be used for matching.
  #'
  #' @return None (modifies the data table in place).

  regex_5_digit <- "\\b\\d{5}\\b"
  valid_rvs_env <- new.env(hash = TRUE, parent = emptyenv())
  for (code in valid_rvs_codes) {
    assign(code, TRUE, envir = valid_rvs_env)
  }

  datatable[, matches := regmatches(col, gregexpr(regex_5_digit, col))]
  datatable[, valid_matches := lapply(
    matches,
    function(x) x[x %in% valid_rvs_codes]
  )]
  datatable[, clin_rvs := mapply(
    function(rvs, matches) unique(c(rvs, matches)),
    clin_rvs, valid_matches,
    SIMPLIFY = FALSE
  )]
}

remove_5_digit_codes <- function(col) {
  #' @title Remove 5-Digit Codes
  #' @description This function removes 5-digit codes from a given column.
  #'
  #' @param col A character vector containing codes.
  #'
  #' @return A modified character vector with 5-digit codes removed.

  # Ensure input is a character vector
  col <- as.character(col) # Convert to character if not already

  # Define regex pattern for 5-digit codes
  regex_5_digit <- "\\b\\d{5}\\b"

  # Use stri_replace_all_regex to remove 5-digit codes
  modified_col <- stri_replace_all_regex(
    col,
    regex_5_digit,
    "",
    vectorize_all = FALSE # Apply replacement across all elements
  )

  return(modified_col)
}

warn_invalid_rvs <- function(matches, valid_rvs_codes) {
  #' @title Warn About Invalid RVS Codes
  #' @description This function identifies and warns about invalid RVS codes found in a set of matches.
  #'
  #' @param matches A list of matched codes to be checked for validity.
  #' @param valid_rvs_codes A vector of valid RVS codes.
  #'
  #' @return A data table of invalid RVS codes and their counts, if any are found; otherwise, an empty data table.

  valid_rvs_env <- new.env(hash = TRUE, parent = emptyenv())
  for (code in valid_rvs_codes) {
    assign(code, TRUE, envir = valid_rvs_env)
  }

  invalid_matches <- lapply(
    matches,
    function(x) x[!vapply(x, exists, logical(1), envir = valid_rvs_env)]
  )
  discarded_codes <- unlist(invalid_matches)
  if (length(discarded_codes) > 0) {
    discarded_table <- data.table(
      CODE = discarded_codes
    )[, .N, by = CODE][order(-N)]
    setnames(discarded_table, c("CODE", "count"))
  } else {
    discarded_table <- data.table()
  }
  return(discarded_table)
}

append_and_remove_rvs <- function(clin_rvs, col, rvs_icd9) {
  #' @title Append and Remove RVS Codes
  #' @description This function appends valid RVS codes to clinical data and removes any invalid 5-digit codes.
  #'
  #' @param clin_rvs A list of clinical RVS codes.
  #' @param col A character vector of codes to be processed.
  #' @param rvs_icd9 A data table of valid RVS codes.
  #'
  #' @return A list containing the modified `clin_rvs`, `col`, and a data table of `discarded_rvs`.

  datatable <- data.table(clin_rvs = clin_rvs, col = col)
  valid_rvs_codes <- rvs_icd9$rvs

  find_and_append_valid_rvs(datatable, valid_rvs_codes)
  datatable[, col := remove_5_digit_codes(col)]
  discarded_rvs <- warn_invalid_rvs(datatable$matches, valid_rvs_codes)

  return(
    list(
      clin_rvs = datatable$clin_rvs,
      col = datatable$col,
      discarded_rvs = discarded_rvs
    )
  )
}

# Function to find the primary diagnosis (PDX) based on the provided logic
find_pdx <- function(clin_c1, clin_c2, clin_icd, acc_pdx_env) {
  #' @title Find primary diagnosis (PDX)
  #'
  #' @description This function finds the primary diagnosis (PDX) based
  #' on the provided clinical codes and acceptable PDX environment.
  #'
  #' @param clin_c1 character The first clinical code.
  #' @param clin_c2 character The second clinical code.
  #' @param clin_icd list The list of clinical ICD codes.
  #' @param acc_pdx_env environment The environment containing
  #' acceptable PDX codes.
  #'
  #' @return list A list containing the PDX and PDX code.

  set.seed(global_seed)

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

  clin_icd <- unlist(clin_icd)

  # Get a list of all SDx that may be chosen as PDx
  pdxs <- unique(clin_icd)
  pdxs <- pdxs[sapply(pdxs, function(x) exists(x, acc_pdx_env))]

  # For those with no acceptable PDx or only 1 acceptable PDx
  if (length(pdxs) == 0) {
    return(list(pdx = NA_character_, pdx_code = 99))
  } else if (length(pdxs) == 1) {
    return(list(pdx = pdxs[1], pdx_code = 3))
  }

  # If there are multiple eligible PDx,
  # see if any are related to the starting letters
  for (cr in c(clin_c1, clin_c2)) {
    if (!is.na(cr)) {
      if (exists(cr, acc_pdx_env)) { # If clin_c* is a valid ICD-10
        # Get starting letter of clin_c*
        starting_letter <- substr(cr, 1, 1)
        # List all valid ICD-10 codes with same starting letter
        starting_codes <- pdxs[substr(pdxs, 1, 1) == starting_letter]
        # If there's only one similar eligible PDx, choose that
        if (length(starting_codes) == 1) {
          return(list(pdx = starting_codes[1], pdx_code = 4))
        }
        # If there are multiple similar eligible PDx
        if (length(starting_codes) > 1) {
          # Obtain the one that most resembles the case rate
          starting_codes <- starting_codes[
            order(sapply(
              starting_codes,
              function(x) check_similarity(cr, x)
            ), decreasing = TRUE)
          ]
          return(list(pdx = starting_codes[1], pdx_code = 5))
        }
      }
    }
  }

  # If there is no related starting letter, choose randomly
  if (length(pdxs) > 0) {
    return(list(pdx = sample(pdxs, 1), pdx_code = 6))
  }

  return(list(pdx = NA_character_, pdx_code = 99))
}

apply_find_pdx <- function(clin_c1, clin_c2, clin_icd, acc_pdx) {
  #' @title Apply find PDX
  #'
  #' @description This function applies the find PDX logic
  #' to a set of clinical codes and acceptable PDX codes.
  #'
  #' @param clin_c1 list The list of first clinical codes.
  #' @param clin_c2 list The list of second clinical codes.
  #' @param clin_icd list The list of clinical ICD codes.
  #' @param acc_pdx character The list of acceptable PDX codes.
  #'
  #' @return list A list containing the PDX and PDX codes
  #' for the input clinical codes.

  acc_pdx_env <- new.env(hash = TRUE, parent = emptyenv())
  for (code in acc_pdx) {
    assign(code, TRUE, envir = acc_pdx_env)
  }

  datatable <- data.table(
    clin_c1 = clin_c1,
    clin_c2 = clin_c2,
    clin_icd = clin_icd
  )

  # Vectorized application of find_pdx function
  find_pdx_vectorized <- function(clin_c1, clin_c2, clin_icd) {
    # Convert lists to characters for easy handling
    clin_c1_char <- sapply(
      clin_c1, function(x) if (is.null(x)) NA_character_ else x
    )
    clin_c2_char <- sapply(
      clin_c2, function(x) if (is.null(x)) NA_character_ else x
    )
    clin_icd_char <- sapply(
      clin_icd, function(x) paste(x, collapse = ",")
    )

    # Initialize result vectors
    pdx <- rep(NA_character_, length(clin_c1))
    pdx_code <- rep(NA_integer_, length(clin_c1))

    # Batch check clin_c1 and clin_c2
    clin_c1_check <- sapply(clin_c1_char, function(x) exists(x, acc_pdx_env))
    clin_c2_check <- sapply(clin_c2_char, function(x) exists(x, acc_pdx_env))

    pdx[clin_c1_check] <- clin_c1_char[clin_c1_check]
    pdx_code[clin_c1_check] <- 1

    clin_c2_only_check <- !clin_c1_check & clin_c2_check
    pdx[clin_c2_only_check] <- clin_c2_char[clin_c2_only_check]
    pdx_code[clin_c2_only_check] <- 2

    # Apply find_pdx function to remaining rows
    remaining_indices <- which(is.na(pdx))
    for (i in remaining_indices) {
      result <- find_pdx(
        clin_c1_char[i], clin_c2_char[i], clin_icd_char[i], acc_pdx_env
      )
      pdx[i] <- result$pdx
      pdx_code[i] <- result$pdx_code
    }

    return(list(pdx = pdx, pdx_code = pdx_code))
  }

  pdx_results <- find_pdx_vectorized(
    datatable$clin_c1,
    datatable$clin_c2,
    datatable$clin_icd
  )
  datatable[, pdx := pdx_results$pdx]
  datatable[, pdx_code := pdx_results$pdx_code]

  return(list(pdx = datatable$pdx, pdx_code = datatable$pdx_code))
}

# # Function to generate date of birth (DOB) vectorized
# generate_dob <- function(bdays, ages, date_adms) {
#   #' @title Generate Date of Birth Vectorized
#   #'
#   #' @description This function generates a vector of dates of birth
#   #' (DOB) based on birthdates, ages, and admission dates.
#   #'
#   #' @param bdays character. A vector of birthdates in string format.
#   #' @param ages numeric. A vector of ages.
#   #' @param date_adms character. A vector of admission dates in string format.
#   #'
#   #' @return character. A vector of dates of birth in "dd/mm/yyyy" format.

#   set.seed(global_seed)

#   require(lubridate)

#   # Ensure ages are numeric
#   ages <- as.numeric(ages)

#   dob <- rep(NA_character_, length(ages))

#   # Use provided birthdates where available
#   valid_bdays_indices <- !is.na(bdays) & bdays != ""
#   dob[valid_bdays_indices] <- format(
#     mdy(bdays[valid_bdays_indices]),
#     "%d/%m/%Y"
#   )

#   # Identify indices where birthdates are missing
#   missing_bday_indices <- which(is.na(bdays) | bdays == "")
#   ref_dates <- mdy(date_adms[missing_bday_indices])

#   # Handle cases where ages are zero:
#   # For age 0, generate a random date within the past 27 days
#   # from the admission date.
#   zero_age_indices <- which(
#     !is.na(ages[missing_bday_indices]) & ages[missing_bday_indices] == 0
#   )
#   dob[missing_bday_indices[zero_age_indices]] <- format(
#     ref_dates[zero_age_indices] - days(
#       sample(
#         1:27, length(zero_age_indices),
#         replace = TRUE
#       )
#     ), "%d/%m/%Y"
#   )

#   # Handle cases where ages are positive:
#   # For positive ages, subtract the truncated age in years and a random
#   # number of days (up to 170) from the admission date.
#   positive_age_indices <- which(
#     !is.na(ages[missing_bday_indices]) & ages[missing_bday_indices] > 0
#   )
#   truncated_ages <- floor(
#     ages[missing_bday_indices][positive_age_indices]
#   )
#   dob[missing_bday_indices[positive_age_indices]] <- format(
#     ref_dates[positive_age_indices] - years(truncated_ages) - days(
#       sample(1:170, length(positive_age_indices), replace = TRUE)
#     ), "%d/%m/%Y"
#   )

#   # # Check that all years for dates are above 1900
#   # years <- year(mdy(dob))
#   # if (any(years < 1900)) {
#   #   stop("Generated dates have years below 1900")
#   # }

#   return(dob)
# }

# Function to generate date of birth (DOB) vectorized
generate_dob <- function(bdays, ages, date_adms) {
  #' @title Generate Date of Birth Vectorized
  #'
  #' @description This function generates a vector of dates of birth
  #' (DOB) based on birthdates, ages, and admission dates.
  #'
  #' @param bdays character. A vector of birthdates in string format.
  #' @param ages numeric. A vector of ages.
  #' @param date_adms character. A vector of admission dates in string format.
  #'
  #' @return character. A vector of dates of birth in "dd/mm/yyyy" format.

  set.seed(global_seed)

  require(lubridate)

  # Ensure ages are numeric
  ages <- as.numeric(ages)

  dob <- rep(NA_character_, length(ages))

  # Use provided birthdates where available
  valid_bdays_indices <- !is.na(bdays) & bdays != ""
  dob[valid_bdays_indices] <- format(
    ymd(bdays[valid_bdays_indices]),
    "%d/%m/%Y"
  )

  # Identify indices where birthdates are missing
  missing_bday_indices <- which(is.na(bdays) | bdays == "")
  ref_dates <- ymd(date_adms[missing_bday_indices])

  # Handle cases where ages are zero:
  # For age 0, generate a random date within the past 27 days
  # from the admission date.
  zero_age_indices <- which(
    !is.na(ages[missing_bday_indices]) & ages[missing_bday_indices] == 0
  )
  dob[missing_bday_indices[zero_age_indices]] <- format(
    ref_dates[zero_age_indices] - days(
      sample(
        1:27, length(zero_age_indices),
        replace = TRUE
      )
    ), "%d/%m/%Y"
  )

  # Handle cases where ages are positive:
  # For positive ages, subtract the truncated age in years and a random
  # number of days (up to 170) from the admission date.
  positive_age_indices <- which(
    !is.na(ages[missing_bday_indices]) & ages[missing_bday_indices] > 0
  )
  truncated_ages <- floor(
    ages[missing_bday_indices][positive_age_indices]
  )
  dob[missing_bday_indices[positive_age_indices]] <- format(
    ref_dates[positive_age_indices] - years(truncated_ages) - days(
      sample(1:170, length(positive_age_indices), replace = TRUE)
    ), "%d/%m/%Y"
  )

  # # Check that all years for dates are above 1900
  # years <- year(ymd(dob))
  # if (any(years < 1900)) {
  #   stop("Generated dates have years below 1900")
  # }

  return(dob)
}
