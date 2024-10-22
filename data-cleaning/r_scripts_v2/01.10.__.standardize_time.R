standardize_time <- function(x) {
  x <- ifelse(is.na(x), "00:00:00", paste0(x, ":00"))
  as.ITime(x)
}
