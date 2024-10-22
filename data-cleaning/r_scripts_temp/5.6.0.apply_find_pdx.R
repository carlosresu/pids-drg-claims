apply_find_pdx <- function(c1, c2, clin_icd, accpdx = acc_pdx) {
  ## Function to apply the PDX finding logic in a vectorized manner

  # Step 1: Create a new environment for accepted PDX codes
  acc_pdx_env <<- new.env(hash = TRUE, parent = emptyenv())

  # Step 2: Populate the environment with accepted PDX codes
  for (code in accpdx) {
    assign(code, TRUE, envir = acc_pdx_env)
  }

  # Step 3: Apply find_pdx_for_row function to all rows
  # See function(s) above
  result <- mapply(find_pdx_for_row, c1, c2, clin_icd, SIMPLIFY = FALSE)

  # Step 4: Extract PDX and PDX codes into vectors
  pdx <- sapply(result, function(x) x$pdx)
  pdx_code <- sapply(result, function(x) x$pdx_code)

  # Return the PDX values and codes
  return(list(pdx = pdx, pdx_code = pdx_code))
}
