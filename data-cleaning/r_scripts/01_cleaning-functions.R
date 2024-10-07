### Helper functions for general data cleaning and processing


##NOTE: Consider renaming this to clean_string_column
clean_column <- function(column_to_clean, na_like_strings, neoplasms_dt) {
  ## runs basic data cleaning steps on a string column
  # column_to_clean: name of column to be cleaned.
  # na_like_strings: vector of strings considered as NA.
  # neoplasms_dt: data.table of substrings where slashes should be preserved
  
  # convert to UTF-8
  column_to_clean <- as.character(column_to_clean)
  cleaned_col <- stri_trans_general(column_to_clean, "Latin-ASCII")
  cleaned_col <- toupper(cleaned_col)

  # remove non-letter and non-digit characters
  cleaned_col <- stri_replace_all_regex(cleaned_col, "[^\\w\\d]+", "")

  # replace NA-like strings with NA
  cleaned_col[cleaned_col %in% na_like_strings] <- NA_character_

  # restore slashes to codes that may pertain to neoplasms
  neopl <- setNames(neoplasms_dt$icd10, gsub("/", "", neoplasms_dt$icd10))
  matched_indices <- match(cleaned_col, names(neopl))
  cleaned_col[!is.na(matched_indices)] <- neopl[matched_indices[!is.na(matched_indices)]]

  return(cleaned_col)
}


collapse_columns <- function(cols_to_process, na_like_strings) {
  ## concatenate entries from multiple string columns into a single one
  # cols_to_process: list of columns to be collapsed
  # na_like_strings: vector of strings considered as NA
  
  # apply the string column cleaning function
  cleaned_columns <- lapply(cols_to_process, function(col) {
    clean_column(col, na_like_strings, neoplasms_dt)
  })
  
  # exclude NA-like strings
  collapsed_column <- do.call(paste, c(cleaned_columns, sep = "||"))
  collapsed_column <- stri_replace_all_regex(collapsed_column, "\\|\\|NA", "")
  collapsed_column <- stri_replace_all_regex(collapsed_column, "NA\\|\\|", "")
  collapsed_column <- stri_replace_all_regex(collapsed_column, "\\|\\|$", "")
  collapsed_column <- stri_replace_all_regex(collapsed_column, "^\\|\\|", "")
  collapsed_column <- ifelse(collapsed_column %in% na_like_strings,
    NA_character_, collapsed_column
  )
  
  # return column with the results
  return(collapsed_column)
}


replace_empty_with_na_python <- function(dt, to_view_checks) {
  ## replace empty strings with NA across a whole data.table,
  ## depending on the data type (i.e. character, factor, list)
  
  char_factor_cols <- names(dt)[sapply(
    dt,
    function(col) is.character(col) || is.factor(col) || is.list(col)
  )]

  replacement_summary <- data.table(
    Column = character(),
    Empty_Replaced = integer(),
    NA_Replaced = integer(),
    Character0_Replaced = integer()
  )

  for (col_name in char_factor_cols) {
    col <- dt[[col_name]]
    if (to_view_checks) {
      empty_count <- sum(col == "", na.rm = TRUE)
      na_count <- sum(col == "NA", na.rm = TRUE)
      char0_count <- sum(col == "character(0)", na.rm = TRUE)
    }

    # Using set to avoid copying
    dt[
      get(
        col_name
      ) == "" | get(col_name) == "NA" | get(col_name) == "character(0)",
      (col_name) := NA
    ]

    if (is.factor(col)) {
      set(dt,
        j = col_name,
        value = factor(dt[[col_name]],
          levels = c(levels(col), NA)
        )
      )
    }

    if (to_view_checks) {
      replacement_summary <- rbind(replacement_summary, data.table(
        Column = col_name,
        Empty_Replaced = empty_count,
        NA_Replaced = na_count,
        Character0_Replaced = char0_count
      ))
    }
  }

  if (to_view_checks) {
    # Filter out rows where all counts are zero
    replacement_summary <- replacement_summary[
      Empty_Replaced > 0 | NA_Replaced > 0 | Character0_Replaced > 0
    ]
  }

  return(
    list(
      # data to return
      return_data = dt,
      # returned summary for checks and outputs
      return_replacement_summary = replacement_summary
    )
  )
}



replace_empty_with_na <- function(dt, to_view_checks = TRUE) {
  ## Replace empty strings in a table with NA (used for data tables in R)
  # dt: table for which empties will be replaced
  # to_view_checks: bool, for viewing summary of replacements
  
  char_factor_cols <- names(dt)[sapply(
    dt,
    function(col) is.character(col) || is.factor(col) || is.list(col)
  )]

  replacement_summary <- data.table(
    Column = character(),
    Empty_Replaced = integer(),
    NA_Replaced = integer(),
    Character0_Replaced = integer()
  )

  for (col_name in char_factor_cols) {
    col <- dt[[col_name]]
    if (to_view_checks) {
      empty_count <- sum(col == "", na.rm = TRUE)
      na_count <- sum(col == "NA", na.rm = TRUE)
      char0_count <- sum(col == "character(0)", na.rm = TRUE)
    }

    # Using set to avoid copying
    dt[
      get(
        col_name
      ) == "" | get(col_name) == "NA" | get(col_name) == "character(0)",
      (col_name) := NA_character_
    ]

    if (is.factor(col)) {
      set(dt,
        j = col_name,
        value = factor(dt[[col_name]],
          levels = c(levels(col), NA)
        )
      )
    }

    if (to_view_checks) {
      replacement_summary <- rbind(replacement_summary, data.table(
        Column = col_name,
        Empty_Replaced = empty_count,
        NA_Replaced = na_count,
        Character0_Replaced = char0_count
      ))
    }
  }

  if (to_view_checks) {
    # Filter out rows where all counts are zero
    replacement_summary <- replacement_summary[
      Empty_Replaced > 0 | NA_Replaced > 0 | Character0_Replaced > 0
    ]
  }

  return(
    list(
      # data to return
      return_data = dt,
      # returned summary for checks and outputs
      return_replacement_summary = replacement_summary
    )
  )
}


replace_empty_with_none <- function(dt, to_view_checks = FALSE) {
  ## Replace empty with None in a table (used in reformatting the data for Python)
  # dt: table for which empties will be replaced
  # to_view_checks: bool, for viewing summary of replacements

  char_factor_cols <- names(dt)[sapply(
    dt,
    function(col) is.character(col) || is.factor(col) || is.list(col)
  )]

  replacement_summary <- data.table(
    Column = character(),
    Empty_Replaced = integer(),
    NA_Replaced = integer(),
    Character0_Replaced = integer()
  )

  for (col_name in char_factor_cols) {
    col <- dt[[col_name]]
    if (to_view_checks) {
      empty_count <- sum(col == "", na.rm = TRUE)
      na_count <- sum(col == "NA", na.rm = TRUE)
      char0_count <- sum(col == "character(0)", na.rm = TRUE)
    }

    # Using set to avoid copying
    dt[
      get(
        col_name
      ) == "" | get(col_name) == "NA" | get(col_name) == "character(0)" | is.na(get(col_name)),
      (col_name) := "None"
    ]

    if (is.factor(col)) {
      set(dt,
        j = col_name,
        value = factor(dt[[col_name]],
          levels = c(levels(col), "None")
        )
      )
    }

    if (to_view_checks) {
      replacement_summary <- rbind(replacement_summary, data.table(
        Column = col_name,
        Empty_Replaced = empty_count,
        NA_Replaced = na_count,
        Character0_Replaced = char0_count
      ))
    }
  }

  if (to_view_checks) {
    # Filter out rows where all counts are zero
    replacement_summary <- replacement_summary[
      Empty_Replaced > 0 | NA_Replaced > 0 | Character0_Replaced > 0
    ]
  }

  return(
    list(
      # data to return
      return_data = dt,
      # returned summary for checks and outputs
      return_replacement_summary = replacement_summary
    )
  )
}

split_to_vector_single <- function(column) {
  ## Splits the elements of a column by "|" and returns a list of vectors
  # column: column to be split
  
  result <- lapply(column, function(x) {
    # if the entry is null, leave it as is 
    if (is.na(x)) {
      return(NA_character_)
    } else {
      
    # split into vector
      return(unlist(strsplit(x, "|", fixed = TRUE)))
    }
  })
  
  # return a list of vectors obtained by splitting the columm
  return(result)
}

split_to_vector <- function(column) {
  ## Split the elements of a column by "||" and returns a list of vectors
  # column: column to be split
  
  result <- lapply(column, function(x) {
    if (is.na(x)) {
      return(NA_character_)
    } else {
      return(unlist(strsplit(x, "||", fixed = TRUE)))
    }
  })
  
  # returns a list of vectors obtained by splitting the columm
  return(result)
}

collapse_and_clean_icd_rvs <- function(dt) {
  ## Collapse and reformat the ICD and RVS columns in a table
  # dt: table for which ICD and RVS columns are cleaned 
  
  # collapse the ICD and RVS codes into clin_icd and clin_rvs, respectively
  dt[, clin_icd := collapse_columns(mget(paste0("clin_icd", 1:12)), na_like_strings)]
  dt[, paste0("clin_icd", 1:12) := NULL]
  
  dt[, clin_rvs := collapse_columns(mget(paste0("clin_rvs", 1:20)), na_like_strings)]
  dt[, paste0("clin_rvs", 1:20) := NULL]
  
  # split up any lumped ICD codes
  dt[, clin_icd := remove_lumped_icd_codes(clin_icd)]
  
  # convert to vectors
  dt[, clin_icd := split_to_vector(clin_icd)]
  # dt[, clin_rvs := remove_lumped_rvs_codes(clin_rvs)]
  dt[, clin_rvs := split_to_vector(clin_rvs)]
  
  # return the data table
  return(dt)
}

transfer_cr_icd <- function(dt) {
  ## Transfer ICD-10 codes in either case rate 1 or 2 to the case's ICD-10 list
  # dt: table for which the case rates will be transferred

  dt[, c1 := split_to_vector(c1)]
  c1_result <- transfer_extra_icd10s_to_clin_icd(
    dt$clin_icd, dt$c1
  )
  dt[, clin_icd := c1_result$clin_icd]
  dt[, c1 := c1_result$col_first]

  dt[, c2 := split_to_vector(c2)]
  c2_result <- transfer_extra_icd10s_to_clin_icd(
    dt$clin_icd, dt$c2
  )
  dt[, clin_icd := c2_result$clin_icd]
  dt[, c2 := c2_result$col_first]

  return(dt)
}


clean_clinical_columns <- function(dt) {
  #' @title Clean clinical columns
  #' @description This function cleans the clinical columns
  #' in the data.table.
  #' @param dt data.table. The data table to be processed.
  #' @return data.table. The processed data table.
  
  dt <- transfer_cr_icd(dt)
  
  c1_rvs_results <- append_and_remove_rvs(
    dt$clin_rvs, dt$c1, rvs_icd9
  )
  dt[, clin_rvs := c1_rvs_results$clin_rvs]
  dt[, c1 := c1_rvs_results$col]
  c1_discarded_rvs <- c1_rvs_results$discarded_rvs

  c2_rvs_results <- append_and_remove_rvs(
    dt$clin_rvs, dt$c2, rvs_icd9
  )
  dt[, clin_rvs := c2_rvs_results$clin_rvs]
  dt[, c2 := c2_rvs_results$col]
  c2_discarded_rvs <- c2_rvs_results$discarded_rvs
  
  # cat(c2_discarded_rvs)
  
  dt[, clin_rvs := lapply(clin_rvs, unique)]
  
  return(
    list(
      # dt to return
      dt = dt,
      # other things to return for checks and outputs
      discard_rvs_one = c1_discarded_rvs,
      discard_rvs_two = c2_discarded_rvs
    )
  )
}