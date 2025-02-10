combine_discarded_rvs_tables <- function(summaries, field) {
  combined <- rbindlist(lapply(summaries, function(s) safe_extract(s, field)), fill = TRUE)
  if (nrow(combined) == 0) {
    return(data.table(CODE = character(), count = integer()))
  }
  return(combined[, .(count = sum(count)), by = CODE][order(-count)])
}
