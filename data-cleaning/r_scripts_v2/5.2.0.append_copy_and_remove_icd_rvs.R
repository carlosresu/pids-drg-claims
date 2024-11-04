# append_copy_and_remove_icd_rvs <- function(clin_rvs, col, rvs_icd9) {
#   ## Ensure both clin_rvs and col are lists of vectors
#   datatable <- data.table(clin_rvs = clin_rvs, col = col)
#   valid_rvs_codes <- rvs_icd9$rvs

#   # Find and append valid RVS codes to clin_rvs
#   datatable[, matches := lapply(col, function(x) {
#     # Identify valid RVS codes within each vector of 'col'
#     valid_codes <- x[x %in% valid_rvs_codes]
#     return(unique(valid_codes))
#   })]

#   # Append valid matches to the existing 'clin_rvs' vector
#   datatable[, clin_rvs := mapply(function(rvs, matches) {
#     unique(c(rvs, matches))
#   }, clin_rvs, matches, SIMPLIFY = FALSE)]

#   # Recursively unlist
#   datatable[, col := lapply(col, function(x) {
#     if (is.null(x) || all(is.na(x))) {
#       return(NA_character_)
#     } else {
#       return(unlist(x, recursive = TRUE, use.names = FALSE))
#     }
#   })]

#   # Trigger warnings for invalid RVS codes
#   valid_rvs_env <- new.env(hash = TRUE, parent = emptyenv()) # Create environment for valid RVS codes

#   # Populate the environment with valid RVS codes
#   for (code in valid_rvs_codes) {
#     assign(code, TRUE, envir = valid_rvs_env)
#   }

#   # Identify invalid RVS codes by checking against the valid RVS environment
#   invalid_matches <- lapply(datatable$matches, function(x) {
#     x[!vapply(x, exists, logical(1), envir = valid_rvs_env)]
#   })

#   # Flatten the list of invalid matches into a single vector
#   discarded_codes <- unlist(invalid_matches)

#   # Create a summary table of discarded codes if any invalid codes are found
#   if (length(discarded_codes) > 0) {
#     discarded_table <- data.table(CODE = discarded_codes)[, .N, by = CODE][order(-N)]
#     setnames(discarded_table, c("CODE", "count"))
#   } else {
#     discarded_table <- data.table()
#   }

#   # Return updated clin_rvs, cleaned col, and discarded RVS codes
#   return(list(
#     clin_rvs = datatable$clin_rvs,
#     col = datatable$col,
#     discarded_rvs = discarded_table
#   ))
# }
append_copy_and_remove_icd_rvs <- function(
    col, clin_rvs, clin_icd, rvscodes = rvs_codes, thai_icd10 = icd_codes, covidrvs = covid_rvs, ph_icds = phil_icds, neoplasmcodes = neoplasm_codes) {
  ## Ensure all input lists are lists of vectors
  datatable <- data.table(clin_rvs = clin_rvs, col = col, clin_icd = clin_icd)
  valid_rvs_codes <- rvscodes
  valid_icd_codes <- thai_icd10
  phil_icds <- ph_icds
  c19_rvs <- covidrvs

  # Step 1: Identify valid RVS codes to move to clin_rvs
  datatable[, matches := lapply(col, function(x) {
    # RVS criteria: 5 numeric digits, start with two letters, or in valid RVS codes/covid RVS
    rvs_codes_in_col <- x[(nchar(x) == 5 & grepl("^[0-9]", x)) |
      grepl("^[A-Z]{2}", x) |
      (x %in% valid_rvs_codes) |
      (x %in% c19_rvs)]
    return(rvs_codes_in_col)
  })]

  # Append valid matches to clin_rvs without affecting existing codes in clin_rvs
  datatable[, clin_rvs := mapply(function(rvs, matches) {
    c(rvs, matches[!matches %in% rvs]) # Add only unique matches
  }, clin_rvs, matches, SIMPLIFY = FALSE)]

  # Step 2: Identify valid ICD codes to move to clin_icd
  datatable[, icd_matches := mapply(function(rvs_vec, icd_vec) {
    # ICD criteria: not exactly 5 digits, does not start with two letters, in valid ICD or phil_icds
    icd_codes_in_rvs <- rvs_vec[(!grepl("^[0-9]{5}$", rvs_vec) &
      !grepl("^[A-Z]{2}", rvs_vec) &
      !grepl("/", rvs_vec)) |
      (rvs_vec %in% valid_icd_codes) |
      (rvs_vec %in% phil_icds)]
    c(icd_vec, icd_codes_in_rvs[!icd_codes_in_rvs %in% icd_vec]) # Add unique codes
  }, clin_rvs, clin_icd, SIMPLIFY = FALSE)]

  # Update clin_icd with identified valid ICD codes
  datatable[, clin_icd := icd_matches]

  # Step 3: Remove RVS codes from col only if they don’t belong there
  datatable[, col := lapply(col, function(x) {
    # Keep in col only those codes that do not meet RVS criteria
    x[!(x %in% matches)]
  })]

  # Step 4: Remove ICD codes from clin_rvs only if they don’t belong there
  datatable[, clin_rvs := lapply(clin_rvs, function(rvs_vec) {
    rvs_vec[(nchar(rvs_vec) == 5 & grepl("^[0-9]", rvs_vec)) |
      grepl("^[A-Z]{2}", rvs_vec) |
      (rvs_vec %in% valid_rvs_codes) |
      (rvs_vec %in% c19_rvs)]
  })]

  # Step 5: Move RVS codes from clin_icd to clin_rvs based on criteria if not already in clin_rvs
  datatable[, clin_rvs := mapply(function(rvs_vec, icd_vec) {
    # Extract RVS codes from clin_icd based on criteria
    rvs_codes_in_icd <- icd_vec[(nchar(icd_vec) == 5 & grepl("^[0-9]", icd_vec)) |
      grepl("^[A-Z]{2}", icd_vec) |
      (icd_vec %in% valid_rvs_codes) |
      (icd_vec %in% c19_rvs)]
    c(rvs_vec, rvs_codes_in_icd[!rvs_codes_in_icd %in% rvs_vec]) # Add unique RVS codes
  }, clin_rvs, clin_icd, SIMPLIFY = FALSE)]

  # Step 6: Remove RVS codes from clin_icd only if they don’t belong there
  datatable[, clin_icd := lapply(clin_icd, function(icd_vec) {
    icd_vec[(!grepl("^[0-9]{5}$", icd_vec) &
      !grepl("^[A-Z]{2}", icd_vec) &
      !grepl("/", icd_vec)) |
      (icd_vec %in% valid_icd_codes) |
      (icd_vec %in% phil_icds)]
  })]

  # Step 7: Recursively unlist elements in col
  datatable[, col := lapply(col, function(x) {
    if (is.null(x) || all(is.na(x))) {
      return(NA_character_)
    } else {
      return(unlist(x, recursive = TRUE, use.names = FALSE))
    }
  })]

  # Step 8: Identify invalid RVS codes
  invalid_matches <- lapply(datatable$matches, function(x) {
    x[!x %in% valid_rvs_codes]
  })
  discarded_codes <- unlist(invalid_matches)

  # Create a summary table for discarded codes if there are any invalid codes
  if (length(discarded_codes) > 0) {
    discarded_table <- data.table(CODE = discarded_codes)[, .N, by = CODE][order(-N)]
    setnames(discarded_table, c("CODE", "count"))
  } else {
    discarded_table <- data.table()
  }

  # Return updated clin_rvs, clin_icd, cleaned col, and discarded RVS codes
  return(list(
    clin_rvs = datatable$clin_rvs,
    clin_icd = datatable$clin_icd,
    col = datatable$col,
    discarded_rvs = discarded_table
  ))
}
