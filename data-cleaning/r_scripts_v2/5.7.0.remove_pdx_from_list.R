remove_pdx_from_list <- function(pdx, lst) {
  if (!is.na(pdx)) {
    # Remove the primary diagnosis from the list
    lst <- setdiff(lst, pdx)
  }
  return(lst)
}
