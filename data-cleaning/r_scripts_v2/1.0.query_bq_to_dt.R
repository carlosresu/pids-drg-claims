query_bq_to_dt <- function(query, max_bq_rows = Inf) {
  # Execute a BigQuery SQL query and return the result as a data.table.
  # max_bq_rows limits the number of rows to download (default is all rows).
  tryCatch(
    # Run the query and download the results as a data.table
    dt <- as.data.table(
      bq_table_download(
        bq_project_query(gcp_proj, query), # Execute the query under project gcp_proj
        n_max = max_bq_rows # Limit the number of rows to download
      )
    ),
    error = function(e) {
      # Catch and re-throw any error with a custom message
      stop(paste("Error querying BigQuery:", e$message))
    }
  )

  # Return the resulting data.table
  return(dt)
}
