# 01.__.query_bq_to_dt.R
query_bq_to_dt <- function(query, max_bq_rows = Inf) {
  tryCatch(
    dt <- as.data.table(
      bq_table_download(bq_project_query(gcp_proj, query), n_max = max_bq_rows)
    ),
    error = function(e) {
      stop(paste("Error querying BigQuery:", e$message))
    }
  )
  return(dt)
}
