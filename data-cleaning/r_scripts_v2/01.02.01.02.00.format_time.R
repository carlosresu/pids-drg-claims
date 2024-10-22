# 01.02.02 Format period to string
format_time <- function(period) {
  # Extract components
  h <- hour(period)
  m <- minute(period)
  s <- second(period)

  # Construct time string with labels
  time_components <- c()
  if (h > 0) {
    time_components <- c(
      time_components, paste0(h, "h")
    )
  }
  if (m > 0 || h > 0) {
    time_components <- c(
      time_components, paste0(m, "m")
    )
  }
  time_components <- c(
    time_components, paste0(s, "s")
  )

  # Join components and return
  time_str <- paste(
    time_components,
    collapse = " "
  )
  return(trimws(time_str))
}
