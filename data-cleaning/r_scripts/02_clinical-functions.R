### Helper functions for remapping/reformatting key columns in the claims


remap_patient_type <- function(pat_type) {
  ## Abbreviate patient types to M (member) and D (dependent)
  # pat_type: column for the patient type

  # remap the column
  remapped_pat_type <- fcase(
    pat_type == "MEMBER", "M",
    pat_type == "DEPENDENT", "D"
  )

  # Check for any unmapped entries and print a warning
  known_types <- c("MEMBER", "DEPENDENT")
  unknown_types <- setdiff(pat_type[!is.na(pat_type)], known_types)

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
  ## Abbreviate the claim status column
  # claim_status: column with the claim payment status

  # remap the column
  remapped_claim_status <- fcase(
    claim_status == "DENIED", "D",
    claim_status == "IN-PROCESS", "I",
    claim_status == "PAID", "G",
    claim_status == "RTH", "R",
    claim_status == "APRV4PAYMENT", "G"
  )

  # check for unmapped claim statuses and print a warning
  known_types <- c("DENIED", "IN-PROCESS", "PAID", "RTH", "APRV4PAYMENT")
  unknown_types <- setdiff(claim_status[!is.na(claim_status)], known_types)

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
  ## Abbreviate the member category column

  # remap the column
  remapped_memcat_parent <- fcase(
    pat_memcat_parent == "DIRECT CONTRIBUTOR", "D",
    pat_memcat_parent == "INDIRECT CONTRIBUTOR", "I"
  )

  # Check for unmapped parent descriptions and print a warning
  known_parents <- c("DIRECT CONTRIBUTOR", "INDIRECT CONTRIBUTOR")
  unknown_parents <- setdiff(pat_memcat_parent[!is.na(pat_memcat_parent)], known_parents)

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
  ## Remap the member category column to the main membership types

  # remap the column
  remapped_memcat_child <- fcase(
    pat_memcat_child == "EMPLOYED PRIVATE", "FORMAL",
    pat_memcat_child == "SELF-EARNING INDIVIDUAL", "INFORMAL",
    pat_memcat_child == "SENIOR CITIZEN", "SENIOR",
    pat_memcat_child == "INDIGENT", "INDIGENT",
    pat_memcat_child == "LIFETIME MEMBER", "LIFETIME",
    pat_memcat_child == "SPONSORED", "SPONSORED",
    pat_memcat_child == "MIGRANT WORKER", "OFW",
    pat_memcat_child == "EMPLOYED GOVERNMENT", "FORMAL",
    pat_memcat_child == "INFORMAL ECONOMY", "INFORMAL",
    pat_memcat_child == "HOUSEHOLD HELP/KASAMBAHAY", "FORMAL",
    pat_memcat_child == "FOREIGN NATIONAL", "OFW",
    pat_memcat_child == "FILIPINOS WITH DUAL CITIZENSHIP / LIVING ABROAD",
    "OFW",
    pat_memcat_child == "SELF EARNING INDIVIDUAL", "INFORMAL",
    pat_memcat_child == "FAMILY DRIVER", "FORMAL",
    # added the following
    pat_memcat_child == "FORMAL ECONOMY", "FORMAL",
    pat_memcat_child == "DIRECT CONTRIBUTOR", "FORMAL",
    pat_memcat_child == "PROFESSIONAL PRACTITIONER", "INFORMAL"
  )

  # Check for unmapped child descriptions and print a warning
  known_children <- c(
    "EMPLOYED PRIVATE", "SELF-EARNING INDIVIDUAL", "SENIOR CITIZEN", "INDIGENT",
    "LIFETIME MEMBER", "SPONSORED", "MIGRANT WORKER", "EMPLOYED GOVERNMENT",
    "INFORMAL ECONOMY", "HOUSEHOLD HELP/KASAMBAHAY", "FOREIGN NATIONAL",
    "FILIPINOS WITH DUAL CITIZENSHIP / LIVING ABROAD",
    "SELF EARNING INDIVIDUAL", "FAMILY DRIVER", "FORMAL ECONOMY",
    "PROFESSIONAL PRACTITIONER", "DIRECT CONTRIBUTOR"
  )

  unknown_children <- setdiff(
    pat_memcat_child[!is.na(pat_memcat_child)],
    known_children
  )

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
  ## Abbreviate the disposition upon discharge
  # clin_discharge: column for the patient type

  remapped_discharge <- fcase(
    clin_discharge == "IMPROVED", 1L,
    clin_discharge == "RECOVERED", 1L,
    clin_discharge == "HOME/DISCHARGED AGAINST MEDICAL ADVICE", 2L,
    clin_discharge == "ABSCONDED", 3L,
    clin_discharge == "TRANSFERRED/REFERRED", 4L,
    clin_discharge == "EXPIRED", 9L,
    clin_discharge == "UNDEFINED", NA_integer_
  )

  # Check for unmapped discharge dispositions and print a warning
  known_dispositions <- c(
    "IMPROVED", "RECOVERED", "HOME/DISCHARGED AGAINST MEDICAL ADVICE",
    "ABSCONDED", "TRANSFERRED/REFERRED", "EXPIRED", "UNDEFINED"
  )

  unknown_dispositions <- setdiff(
    clin_discharge[!is.na(clin_discharge)],
    known_dispositions
  )

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
  ## Remap the categorical columns in the inpatient data

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
  ## Takes a column and separates out ICD-10 codes using "||"
  ## been lumped into a single string

  modified_column <- stri_replace_all_regex(
    column, "(?<=\\d)(?=[A-Za-z])", "||",
    opts_regex = stri_opts_regex()
  )

  return(modified_column)
}

remove_lumped_rvs_codes <- function(column) {
  ## Separates out lumped RVS codes by splitting into chunks of 5 chars each

  # Define a helper function to process each code
  split_rvs_codes_helper <- function(code) {
    if (is.na(code) || code == "" || is.null(code)) {
      return(NA_character_) # Return NA if input is NA, empty, or NULL
    }

    # Keep only alphanumeric characters
    code_clean <- gsub("\\|", "", code)
    code_clean <- gsub("[^A-Z0-9]", "", code_clean)

    # Check if the cleaned code length is a multiple of 5 characters
    if (nchar(code_clean) == 0) {
      return(NA_character_) # Return NA if the code length is not a multiple of 5
    } else if (nchar(code_clean) %% 5 != 0) {
      message(paste0("Total length of concatenated RVS codes is not a multiple of 5 characters: ", code_clean))
      return(NA_character_)
    } else {
      # Insert the separator \\|\\| between every 5 characters
      modified_code <- gsub("(.{5})", "\\1\\|\\|", code_clean)

      # Remove the trailing separator (\\|\\|) if present
      modified_code <- gsub("\\|\\|$", "", modified_code)

      return(modified_code)
    }
  }

  # Apply the helper function to each element in the column
  modified_column <- sapply(as.character(column), split_rvs_codes_helper, USE.NAMES = FALSE)

  return(modified_column)
}

remove_lumped_icd9_codes <- function(column) {
  ## Separates out lumped ICD9 codes by splitting into chunks of 4 chars each

  # Define a helper function to process each code
  split_rvs_codes_helper_icd9 <- function(code) {
    if (is.na(code) || code == "" || is.null(code)) {
      return(NA_character_) # Return NA if input is NA, empty, or NULL
    }

    # Remove all '|' characters
    code_clean <- gsub("\\|", "", code)

    # Ensure that only alphanumeric characters are kept
    code_clean <- gsub("[^A-Z0-9]", "", code_clean)

    # Check if the cleaned code length is a multiple of 4 characters
    if (nchar(code_clean) == 0) {
      return(NA_character_) # Return NA if the code length is not a multiple of 4
    } else if (nchar(code_clean) %% 4 != 0) {
      message(paste0("Total length of concatenated RVS codes is not a multiple of 4 characters: ", code_clean))
      return(NA_character_)
    } else {
      # Insert the separator \\|\\| between every 4 characters
      modified_code <- gsub("(.{4})", "\\1\\|\\|", code_clean)

      # Remove the trailing separator (\\|\\|) if present
      modified_code <- gsub("\\|\\|$", "", modified_code)

      return(modified_code)
    }
  }

  # Apply the helper function to each element in the column
  modified_column <- sapply(as.character(column), split_rvs_codes_helper_icd9,
    USE.NAMES = FALSE
  )

  return(modified_column)
}


transfer_extra_icd10s_to_clin_icd <- function(clin_icd, col) {
  ## Transfer extra ICD-10 codes from a column to the compilation of ICD-10 codes for the case
  # clin_icd: column with all the ICD-10 codes
  # col: column with the possible extra ICD-10 codes

  # I dont get how this works
  clin_icd <- lapply(clin_icd, function(x) if (is.null(x)) character() else x)
  col_first <- lapply(col, function(x) x[1])
  clin_icd <- mapply(function(icd, c1) {
    c(icd, c1[-1])
  }, clin_icd, col, SIMPLIFY = FALSE)

  return(list(clin_icd = clin_icd, col_first = col_first))
}


get_unique_icd_codes <- function(c1, c2, clin_icd) {
  ## obtains list of all unique ICD-10 codes across all cases and columns

  icds <- unique(c(unlist(c1), unlist(c2), unlist(clin_icd)))
  icds <- icds[!is.na(icds)]
  return(icds)
}


create_thai_icd10_environment <- function(thai_icd10_codes) {
  ## create environment for Thai ICD-10 codes

  thai_icd10_env <- list2env(
    setNames(as.list(rep(TRUE, length(thai_icd10_codes))), thai_icd10_codes)
  )
  return(thai_icd10_env)
}


find_direct_icd_matches <- function(icds, thai_icd10_env) {
  ## Identify ICD-10 codes with exact matches in the Thai ICD-10 library
  # icds: list of ICD-10 codes to be cross-checked
  # thai_icd10_env: environment of Thai ICD-10 codes

  # what does this do?
  direct_matches <- mget(
    icds, thai_icd10_env,
    ifnotfound = as.list(rep(FALSE, length(icds)))
  )

  # what does this do?
  direct_match_codes <- names(
    unlist(direct_matches[unlist(direct_matches) == TRUE])
  )
  return(direct_match_codes)
}


generate_icd10_mapping <- function(icds, thai_icd10_env, neoplasms_env) {
  ## Map ICD-10 codes to their closest equivalents in the Thai ICD-10 library
  # icds: list of ICD-10 codes to be mapped
  # thai_icd10_env: environment with Thai ICD-10 codes
  # neoplasms_env: environment with neoplasm ICD codes

  icd_mapping <- list()
  modified_count <- 0

  # loop through the codes in icds
  for (d in icds) {
    d <- str_trim(d)

    # if it has an exact match, then map directly
    if (exists(d, thai_icd10_env)) {
      icd_mapping[[d]] <- d
    } else if (!exists(d, neoplasms_env) && grepl("[A-Za-z]", d) && grepl("[0-9]", d)) {
      # if the code is not for a neoplasm, try adding 9 and see if there is a match
      if (nchar(d) == 3 && exists(paste0(d, "9"), thai_icd10_env)) {
        icd_mapping[[d]] <- paste0(d, "9")
        modified_count <- modified_count + 1
      } else if (nchar(d) >= 4) {
        # otherwise, try trimming by a digit until there is a match
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


apply_icd10_mapping_to_columns <- function(c1, c2, clin_icd, icd10_env) {
  ## maps ICD-10 codes to the given columns using the provided environment.
  # c1: column for case rate 1
  # c2: column for case rate 2
  # icd10_env: environment with ICD-10 codes

  # what does this do?
  map_icd10_helper <- function(codes) {
    mapped <- mget(codes, icd10_env, ifnotfound = as.list(codes))
    return(unname(unlist(mapped)))
  }

  # change this to a for-loop
  c1_mapped <- lapply(c1, map_icd10_helper)
  c2_mapped <- lapply(c2, map_icd10_helper)
  clin_icd_mapped <- lapply(clin_icd, map_icd10_helper)

  return(
    list(
      c1 = c1_mapped,
      c2 = c2_mapped,
      clin_icd = clin_icd_mapped
    )
  )
}

# Function to ensure unique ICD codes
ensure_unique_icd_codes <- function(c1, c2, clin_icd) {
  #' @title Ensure Unique ICD Codes
  #'
  #' @description This function ensures that ICD codes are unique
  #' within and across clinical columns.
  #'
  #' @param c1 list. The clinical column 1 ICD codes.
  #' @param c2 list. The clinical column 2 ICD codes.
  #' @param clin_icd list. The clinical ICD codes.
  #'
  #' @return list. A list containing the deduplicated clinical columns.

  # Convert lists to data.table for efficient processing
  datatable <- data.table(
    c1 = c1,
    c2 = c2,
    clin_icd = clin_icd
  )

  # Deduplicate each column
  # datatable[, c1 := lapply(c1, unique)]
  # datatable[, c2 := lapply(c2, unique)]
  # datatable[, clin_icd := lapply(clin_icd, unique)]

  # Remove entries in clin_icd that are in c1 or c2
  datatable[, clin_icd := Map(function(c1, c2, icd) {
    setdiff(icd, union(c1, c2))
  }, c1, c2, clin_icd)]

  # # Remove entries in c1 that are in c2
  # datatable[, c1 := Map(function(c1, c2) {
  #   setdiff(c1, c2)
  # }, c1, c2)]
  # # Remove entries in c2 that are in c1
  # datatable[, c2 := Map(function(c1, c2) {
  #   setdiff(c2, c1)
  # }, c1, c2)]

  return(
    list(
      c1 = datatable$c1,
      c2 = datatable$c2,
      clin_icd = datatable$clin_icd
    )
  )
}

split_rvs_codes <- function(rvs_icd9) {
  ## Split the RVS codes into those with DRG
  # rvs_icd9: table containing RVS codes and a logical column `is_drg`.

  with_drg <- rvs_icd9[is_drg == TRUE]
  without_drg <- rvs_icd9[!rvs %in% with_drg$rvs]
  return(list(with_drg = with_drg, without_drg = without_drg))
}


create_rvs_map_lists <- function(with_drg) {
  ## Create two lists for mapping RVS codes to ICD-9-CM codes: one for solo mappings and one for multi-mappings.
  # with_drg: table of RVS codes with corresponding DRG, ordered by `rvs` and `is_drg`.

  setorder(with_drg, rvs, -is_drg)
  unique_rvs <- with_drg[, .(icd9cm_list = list(icd9cm)), by = rvs]
  solo <- unique_rvs[lengths(icd9cm_list) == 1]
  list_mapped <- unique_rvs[lengths(icd9cm_list) > 1]

  rvs_map_solo <- setNames(solo$icd9cm_list, solo$rvs)
  rvs_map_list <- setNames(list_mapped$icd9cm_list, list_mapped$rvs)

  return(list(rvs_map_list = rvs_map_list, rvs_map_solo = rvs_map_solo))
}


get_icd9_codes <- function(clin_rvs, rvs_map_solo_env) {
  ## Maps a column containing RVS codes to ICD-9-CM
  # clin_rvs: list of clinical RVS codes.
  # rvs_map_solo_env: environment containing mappings from RVS codes to ICD-9 codes.

  # loop through each of the rows of clin_rvs
  lapply(clin_rvs, function(x) {
    codes <- unlist(x)

    # map all codes with ICD-9-CM equivalents
    mappable <- codes[
      !is.na(mget(codes, envir = rvs_map_solo_env, ifnotfound = NA))
    ]
    if (length(mappable) > 0) {
      # for those without any, leave as is
      unique(unlist(mget(mappable, envir = rvs_map_solo_env)))
    } else {
      NA_character_
    }
  })
}

find_and_append_valid_rvs <- function(datatable, valid_rvs_codes) {
  ## Identify valid RVS codes in a data table and append them to existing clinical RVS codes
  # datatable: table containing `clin_rvs` and `col` columns
  # valid_rvs_codes: vector of valid RVS codes to be used for matching

  # what does this do?
  regex_5_digit <- "\\b\\d{5}\\b"
  valid_rvs_env <- new.env(hash = TRUE, parent = emptyenv())

  # what does this do?
  for (code in valid_rvs_codes) {
    assign(code, TRUE, envir = valid_rvs_env)
  }

  # what does this do?
  datatable[, matches := regmatches(col, gregexpr(regex_5_digit, col))]
  datatable[, valid_matches := lapply(matches, function(x) x[x %in% valid_rvs_codes])]

  # what does this do?
  datatable[, clin_rvs := mapply(
    function(rvs, matches) unique(c(rvs, matches)),
    clin_rvs, valid_matches,
    SIMPLIFY = FALSE
  )]
}


remove_5_digit_codes <- function(col) {
  ## remove 5-digit codes from a given column
  # col: column with codes

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
  ## Triggers warnings for invalid RVS codes
  # matches: list of matched codes to be checked for validity
  # valid_rvs_codes: vector of valid RVS codes

  # what does this do?
  valid_rvs_env <- new.env(hash = TRUE, parent = emptyenv())
  for (code in valid_rvs_codes) {
    assign(code, TRUE, envir = valid_rvs_env)
  }

  # what does this do?
  invalid_matches <- lapply(
    matches,
    function(x) x[!vapply(x, exists, logical(1), envir = valid_rvs_env)]
  )

  # what does this do?
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
  ## Appends RVS codes from a column to the main RVS column
  # clin_rvs: list of RVS codes
  # col: column of codes to be processed
  # rvs_icd9: table mapping RVS to ICD-9-CM

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
find_pdx <- function(c1, c2, clin_icd, acc_pdx_env) {
  set.seed(global_seed)

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

  # Unlist clin_icd properly
  clin_icd <- unlist(strsplit(clin_icd, "\\|")) # Split by '|' if needed

  # Get a list of all ICDs that are acceptable as PDx
  pdxs <- unique(clin_icd)
  pdxs <- pdxs[sapply(pdxs, function(x) exists(x, acc_pdx_env))]

  # For cases with no or one acceptable PDx
  if (length(pdxs) == 0) {
    return(list(pdx = NA_character_, pdx_code = 99))
  } else if (length(pdxs) == 1) {
    return(list(pdx = pdxs[1], pdx_code = 3))
  }

  # If there are multiple eligible PDx, check c1 and c2 first
  for (cr in c(c1, c2)) {
    if (!is.na(cr)) {
      if (exists(cr, acc_pdx_env)) {
        # what does this do?
        starting_letter <- substr(cr, 1, 1)
        starting_codes <- pdxs[substr(pdxs, 1, 1) == starting_letter]
        if (length(starting_codes) == 1) {
          # what does this do?
          return(list(pdx = starting_codes[1], pdx_code = 4))
        } else if (length(starting_codes) > 1) {
          # what does this do?
          starting_codes <- starting_codes[
            order(sapply(starting_codes, function(x) check_similarity(cr, x)),
              decreasing = TRUE
            )
          ]
          return(list(pdx = starting_codes[1], pdx_code = 5))
        }
      }
    }
  }

  # Choose randomly if no match based on starting letters
  if (length(pdxs) > 0) {
    return(list(pdx = sample(pdxs, 1), pdx_code = 6))
  }

  return(list(pdx = NA_character_, pdx_code = 99))
}

# Function to apply the find_pdx logic to a data.table
apply_find_pdx <- function(c1, c2, clin_icd, acc_pdx) {
  acc_pdx_env <- new.env(hash = TRUE, parent = emptyenv())
  for (code in acc_pdx) {
    assign(code, TRUE, envir = acc_pdx_env)
  }

  datatable <- data.table(
    c1 = c1,
    c2 = c2,
    clin_icd = clin_icd
  )

  find_pdx_vectorized <- function(c1, c2, clin_icd) {
    c1_char <- sapply(c1, function(x) if (is.null(x)) NA_character_ else x)
    c2_char <- sapply(c2, function(x) if (is.null(x)) NA_character_ else x)
    clin_icd_char <- sapply(clin_icd, function(x) paste(x, collapse = "|")) # Join with '|'

    pdx <- rep(NA_character_, length(c1))
    pdx_code <- rep(NA_integer_, length(c1))

    c1_check <- sapply(c1_char, function(x) exists(x, acc_pdx_env))
    c2_check <- sapply(c2_char, function(x) exists(x, acc_pdx_env))

    pdx[c1_check] <- c1_char[c1_check]
    pdx_code[c1_check] <- 1

    c2_only_check <- !c1_check & c2_check
    pdx[c2_only_check] <- c2_char[c2_only_check]
    pdx_code[c2_only_check] <- 2

    remaining_indices <- which(is.na(pdx))
    for (i in remaining_indices) {
      result <- find_pdx(
        c1_char[i], c2_char[i], clin_icd_char[i], acc_pdx_env
      )
      pdx[i] <- result$pdx
      pdx_code[i] <- result$pdx_code
    }

    return(list(pdx = pdx, pdx_code = pdx_code))
  }

  pdx_results <- find_pdx_vectorized(
    datatable$c1,
    datatable$c2,
    datatable$clin_icd
  )
  datatable[, pdx := pdx_results$pdx]
  datatable[, pdx_code := pdx_results$pdx_code]

  return(list(pdx = datatable$pdx, pdx_code = datatable$pdx_code))
}

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

  set.seed(global_seed) # Ensure reproducibility
  require(lubridate)

  # Ensure ages are numeric
  ages <- as.numeric(ages)

  # Initialize DOB vector with NA
  dob <- rep(NA_character_, length(ages))

  # 1. Use provided birthdates where available
  valid_bdays_indices <- !is.na(bdays) & bdays != ""
  dob[valid_bdays_indices] <- format(
    ymd(bdays[valid_bdays_indices]), # Convert valid birthdates
    "%d/%m/%Y"
  )

  # 2. Handle cases where birthdates are missing
  missing_bday_indices <- which(is.na(bdays) | bdays == "")
  ref_dates <- ymd(date_adms[missing_bday_indices]) # Admission dates

  # 3. Handle age == 0
  zero_age_indices <- which(
    !is.na(ages[missing_bday_indices]) & ages[missing_bday_indices] == 0
  )
  if (length(zero_age_indices) > 0) {
    dob[missing_bday_indices[zero_age_indices]] <- format(
      ref_dates[zero_age_indices] - days(
        sample(1:27, length(zero_age_indices), replace = TRUE) # Random days within the past month
      ), "%d/%m/%Y"
    )
  }

  # 4. Handle positive ages (no random days)
  positive_age_indices <- which(
    !is.na(ages[missing_bday_indices]) & ages[missing_bday_indices] > 0
  )
  if (length(positive_age_indices) > 0) {
    truncated_ages <- floor(ages[missing_bday_indices][positive_age_indices]) # Truncate ages
    dob[missing_bday_indices[positive_age_indices]] <- format(
      ref_dates[positive_age_indices] - years(truncated_ages), "%d/%m/%Y" # Subtract exact age in years
    )
  }

  return(dob)
}
