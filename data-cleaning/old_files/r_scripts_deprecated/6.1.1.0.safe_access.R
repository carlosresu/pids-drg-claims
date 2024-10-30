safe_access <- function(s, field) {
  if (is.list(s) && field %in% names(s)) s[[field]] else NULL
}
