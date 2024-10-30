extract_modified_matches <- function(summaries) {
  all_modified_matches <- list()
  all_modified_match <- list()

  for (summary in summaries) {
    result <- safe_access(summary, "modified_matches")
    if (is.list(result) && all(c("modified_matches", "modified_match") %in% names(result))) {
      all_modified_matches <- append(all_modified_matches, result$modified_matches)
      all_modified_match <- append(all_modified_match, result$modified_match)
    }
  }

  data.table(
    modified_matches = safe_unlist(all_modified_matches),
    modified_match = safe_unlist(all_modified_match)
  )[, .(count = .N), by = .(modified_matches, modified_match)]
}
