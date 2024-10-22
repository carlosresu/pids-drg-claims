combine_chunk_summaries <- function(
    summaries) {
  #' @title Combine Chunk Summaries
  #'
  #' @description This function combines summaries from
  #' multiple chunks into one summary.
  #'
  #' @param summaries list. A list of results from
  #' summaries from parallel processing.
  #' @return list. The combined summary.

  combined_summary <- combine_summaries(summaries)
  return(combined_summary)
}
