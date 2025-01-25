############################# OVERALL TODO #############################
# TO-DO's:
# 0. [DONE] Make a new branch
# 1. [DONE] Separate all nested functions
#    (including those nested within nested functions)
# 2. [DONE] Mapping code should process one column at a time, not all columns at once.
# 3. [DONE] Separate out checks from functions and return statements
############################# OVERALL TODO #############################

############################# SPECIFIC TODO #############################
# 1. document!!!!!!
# 2. rearrange
# 3. test
############################# SPECIFIC TODO #############################

############################# WHAT IS A DOCSTRING? #############################
# Docstring that covers the bigger picture of the function
# Such as why it exists, what it intends to do, and why it was written
############################# WHAT IS A DOCSTRING? #############################

# Step 1: Helper function to check similarity between two strings
check_similarity <- function(x, y) {
  min_len <- min(nchar(x), nchar(y))
  sum(substr(x, 1, min_len) == substr(y, 1, min_len))
}

# Step 2: Filter ICD codes based on exclusion criteria
filter_icds <- function(codes) {
  codes <- codes[!is.na(codes) & !grepl("^[0-9]", codes) &
    !grepl("^[A-Z]{2}", codes) & !grepl("/", codes) &
    !(codes %chin% neoplasm_codes) & !(codes %chin% rvs_codes) &
    !(codes %chin% covidrvs)]
  codes[codes %chin% acc_pdx_set]
}

find_pdx <- function(
    # inputs:
    c1_split = filter_icds(unlist(strsplit(c1, "\\|"))),
    c2_split = filter_icds(unlist(strsplit(c2, "\\|"))),
    clin_icd_split = filter_icds(unlist(strsplit(clin_icd, "\\|"))),
    # dependencies:
    acc_pdx_set = unique(acc_pdx),
    neoplasm_codes = unique(neoplasms_dt_actual$icd10),
    rvs_codes = unique(acr_rvs$rvs),
    covidrvs = unique(covid_rvs),
    seed = global_seed) {
  # Given a set of dependencies (see above), and a set of input columns (icds),
  # find the appropriate PDx for the specified case. It does this by filtering
  # the codes first thru the sieves that are the dependencies above, then apply
  # the find PDx algorithm, which is as follows:
  # Algorithm:
  # 1. Check if case rate 1 or 2 is a valid PDx,
  # 2. Check if
  find_pdx_algorithm <- function(cr1, cr2, cicd) {
    # Step A: Check if any element in cr1 or cr2 is an accepted PDX
    for (cr_list in list(cr1, cr2)) {
      if (length(cr_list) > 0) {
        return(list(
          pdx = cr_list[1],
          pdx_code = ifelse(cr_list[1] %in% cr1, 1, 2)
        ))
      }
    }

    # Step B: Find accepted PDX from cicd
    if (length(cicd) > 0) {
      pdxs <- cicd
    } else {
      return(list(
        pdx = NA_character_,
        pdx_code = 99
      ))
    }

    # Step C: Handle cases with only one accepted PDX
    if (length(pdxs) == 1) {
      return(list(
        pdx = pdxs[1],
        pdx_code = 3
      ))
    }

    # Step D: Check for matching starting letters in cr1 and cr2
    for (cr_list in list(cr1, cr2)) {
      for (cr in cr_list) {
        starting_codes <- pdxs[substr(pdxs, 1, 1) == substr(cr, 1, 1)]
        if (length(starting_codes) == 1) {
          return(list(
            pdx = starting_codes[1],
            pdx_code = 4
          ))
        } else if (length(starting_codes) > 1) {
          best_match <- starting_codes[which.max(
            sapply(starting_codes, check_similarity, y = cr)
          )]
          return(list(
            pdx = best_match,
            pdx_code = 5
          ))
        }
      }
    }

    # Step E: Pick a random PDX if no match is found
    set.seed(seed)
    list(
      pdx = sample(pdxs, 1),
      pdx_code = 6
    )
  }

  # Step 3: Apply the PDX finding logic row-wise
  algo_result <- mapply(
    find_pdx_algorithm,
    cr1 = c1_split, cr2 = c2_split, cicd = clin_icd_split,
    SIMPLIFY = FALSE
  )

  # Step 4: Return the PDX values and codes
  return(list(
    pdx = sapply(algo_result, `[[`, "pdx"),
    pdx_code = sapply(algo_result, `[[`, "pdx_code")
  ))
}
