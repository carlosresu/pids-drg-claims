find_pdx <- function(c1, c2, clin_icd, accpdx = acc_pdx,
                     neoplasmsdtactual = neoplasms_dt_actual,
                     acrrvs = acr_rvs, covidrvs = covid_rvs) {
  # Step 1: Define sets for filtering and accepted PDX codes
  acc_pdx_set <- unique(accpdx)
  neoplasm_codes <- unique(neoplasmsdtactual$icd10)
  rvs_codes <- unique(acrrvs$rvs)

  # Step 2: Helper function to check similarity between two strings
  check_similarity <- function(x, y) {
    min_len <- min(nchar(x), nchar(y))
    sum(substr(x, 1, min_len) == substr(y, 1, min_len))
  }

  # Step 3: Filter ICD codes based on exclusion criteria
  filter_icds <- function(codes) {
    codes <- codes[!is.na(codes) & !grepl("^[0-9]", codes) &
      !grepl("^[A-Z]{2}", codes) & !grepl("/", codes) &
      !(codes %chin% neoplasm_codes) & !(codes %chin% rvs_codes) &
      !(codes %chin% covidrvs)]
    codes[codes %chin% acc_pdx_set]
  }

  # Step 4: Apply the PDX finding logic row-wise
  result <- mapply(function(c1, c2, clin_icd) {
    # Split c1, c2, and clin_icd by '|' if necessary
    c1_split <- filter_icds(unlist(strsplit(c1, "\\|")))
    c2_split <- filter_icds(unlist(strsplit(c2, "\\|")))
    clin_icd_split <- filter_icds(unlist(strsplit(clin_icd, "\\|")))

    # Step 5: Check if any element in c1 or c2 is an accepted PDX
    for (cr_list in list(c1_split, c2_split)) {
      if (length(cr_list) > 0) {
        return(list(pdx = cr_list[1], pdx_code = ifelse(cr_list[1] %in% c1_split, 1, 2)))
      }
    }

    # Step 6: Find accepted PDX from clin_icd_split
    if (length(clin_icd_split) > 0) {
      pdxs <- clin_icd_split
    } else {
      return(list(pdx = NA_character_, pdx_code = 99))
    }

    # Step 7: Handle cases with only one accepted PDX
    if (length(pdxs) == 1) {
      return(list(pdx = pdxs[1], pdx_code = 3))
    }

    # Step 8: Check for matching starting letters in c1 and c2
    for (cr_list in list(c1_split, c2_split)) {
      for (cr in cr_list) {
        starting_codes <- pdxs[substr(pdxs, 1, 1) == substr(cr, 1, 1)]
        if (length(starting_codes) == 1) {
          return(list(pdx = starting_codes[1], pdx_code = 4))
        } else if (length(starting_codes) > 1) {
          best_match <- starting_codes[which.max(sapply(starting_codes, check_similarity, y = cr))]
          return(list(pdx = best_match, pdx_code = 5))
        }
      }
    }

    # Step 9: Pick a random PDX if no match is found
    list(pdx = sample(pdxs, 1), pdx_code = 6)
  }, c1, c2, clin_icd, SIMPLIFY = FALSE)

  # Step 10: Extract PDX and PDX codes into vectors
  pdx <- sapply(result, `[[`, "pdx")
  pdx_code <- sapply(result, `[[`, "pdx_code")

  # Step 11: Return the PDX values and codes
  list(pdx = pdx, pdx_code = pdx_code)
}
