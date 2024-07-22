# Function to find the primary diagnosis (PDX) based on the provided logic
find_pdx <- function(clin_c1, clin_c2, clin_icd, acc_pdx_env) {
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

  # Convert clin_icd to a character vector
  clin_icd <- unlist(strsplit(clin_icd, ","))

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
            order(sapply(starting_codes, function(x) check_similarity(cr, x)), decreasing = TRUE)
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

# Function to apply find_pdx to a dataset
apply_find_pdx <- function(clin_c1, clin_c2, clin_icd, acc_pdx) {
  acc_pdx_env <- new.env(hash = TRUE, parent = emptyenv())
  for (code in acc_pdx) {
    assign(code, TRUE, envir = acc_pdx_env)
  }

  dt <- data.table(
    clin_c1 = sapply(clin_c1, toString),
    clin_c2 = sapply(clin_c2, toString),
    clin_icd = sapply(clin_icd, function(icds) paste(icds, collapse = ","))
  )

  # Helper function to check existence in acc_pdx_env
  exists_in_acc_pdx_env <- function(x) {
    sapply(x, function(code) exists(code, acc_pdx_env))
  }

  # Initialize pdx and pdx_code columns
  dt[, `:=`(pdx = NA_character_, pdx_code = NA_integer_)]

  # Batch check clin_c1 and clin_c2
  dt[
    is.na(pdx) & !is.na(clin_c1) & exists_in_acc_pdx_env(clin_c1),
    `:=`(pdx = clin_c1, pdx_code = 1)
  ]
  dt[
    is.na(pdx) & !is.na(clin_c2) & exists_in_acc_pdx_env(clin_c2),
    `:=`(pdx = clin_c2, pdx_code = 2)
  ]

  # Apply find_pdx function to remaining rows
  remaining_rows <- dt[is.na(pdx)]
  if (nrow(remaining_rows) > 0) {
    pdx_results <- remaining_rows[,
      {
        result <- find_pdx(clin_c1, clin_c2, clin_icd, acc_pdx_env)
        .(pdx = result$pdx, pdx_code = result$pdx_code)
      },
      by = .(row_id = .I)
    ]

    dt[remaining_rows$row_id, `:=`(pdx = pdx_results$pdx, pdx_code = pdx_results$pdx_code)]
  }

  return(list(pdx = dt$pdx, pdx_code = dt$pdx_code))
}
