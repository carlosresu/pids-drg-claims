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
  # Initialize a data.table
  datatable <- data.table(clin_rvs = clin_rvs, col = col, clin_icd = clin_icd)
  valid_rvs_codes <- rvscodes
  valid_icd_codes <- thai_icd10
  phil_icds <- ph_icds
  c19_rvs <- covidrvs

  # Step 1: Detect valid RVS codes in 'col'
  datatable[, detected_rvs := lapply(col, function(x) {
    x[(nchar(x) == 5 & grepl("^[0-9]", x)) |
      grepl("^[A-Z]{2}", x) |
      (x %in% valid_rvs_codes) |
      (x %in% c19_rvs)]
  })]

  # Move detected RVS codes to 'clin_rvs' if not already present
  datatable[, clin_rvs := mapply(function(rvs, detected) {
    c(rvs, detected[!detected %in% rvs])
  }, clin_rvs, detected_rvs, SIMPLIFY = FALSE)]

  # Remove detected RVS codes from 'col'
  datatable[, col := lapply(col, function(x) setdiff(x, unlist(detected_rvs)))]

  # Step 2: Detect valid ICD codes in 'clin_rvs' for transfer to 'clin_icd'
  datatable[, detected_icd_in_rvs := lapply(clin_rvs, function(x) {
    x[(!grepl("^[0-9]{5}$", x) & !grepl("^[A-Z]{2}", x) & !grepl("/", x)) |
      (x %in% valid_icd_codes) |
      (x %in% phil_icds)]
  })]

  # Move detected ICD codes to 'clin_icd' if not already present
  datatable[, clin_icd := mapply(function(icd, detected) {
    c(icd, detected[!detected %in% icd])
  }, clin_icd, detected_icd_in_rvs, SIMPLIFY = FALSE)]

  # Remove detected ICD codes from 'clin_rvs'
  datatable[, clin_rvs := lapply(clin_rvs, function(x) setdiff(x, unlist(detected_icd_in_rvs)))]

  # Step 3: Detect RVS codes in 'clin_icd' that need to be moved to 'clin_rvs'
  datatable[, detected_rvs_in_icd := lapply(clin_icd, function(x) {
    x[(nchar(x) == 5 & grepl("^[0-9]", x)) |
      grepl("^[A-Z]{2}", x) |
      (x %in% valid_rvs_codes) |
      (x %in% c19_rvs)]
  })]

  # Move detected RVS codes to 'clin_rvs' if not already present
  datatable[, clin_rvs := mapply(function(rvs, detected) {
    c(rvs, detected[!detected %in% rvs])
  }, clin_rvs, detected_rvs_in_icd, SIMPLIFY = FALSE)]

  # Remove detected RVS codes from 'clin_icd'
  datatable[, clin_icd := lapply(clin_icd, function(x) setdiff(x, unlist(detected_rvs_in_icd)))]

  # Step 4: Detect ICD codes in 'col' that need to be moved to 'clin_icd'
  datatable[, detected_icd_in_col := lapply(col, function(x) {
    x[(!grepl("^[0-9]{5}$", x) & !grepl("^[A-Z]{2}", x) & !grepl("/", x)) |
      (x %in% valid_icd_codes) |
      (x %in% phil_icds)]
  })]

  # Move detected ICD codes to 'clin_icd' if not already present
  datatable[, clin_icd := mapply(function(icd, detected) {
    c(icd, detected[!detected %in% icd])
  }, clin_icd, detected_icd_in_col, SIMPLIFY = FALSE)]

  # Remove detected ICD codes from 'col'
  datatable[, col := lapply(col, function(x) setdiff(x, unlist(detected_icd_in_col)))]

  # Step 5: Recursively unlist elements in col
  datatable[, col := lapply(col, function(x) {
    if (is.null(x) || all(is.na(x))) {
      return(NA_character_)
    } else {
      return(unlist(x, recursive = TRUE, use.names = FALSE))
    }
  })]

  # Step 6: Identify invalid RVS codes
  invalid_matches <- lapply(datatable$detected_rvs, function(x) {
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
