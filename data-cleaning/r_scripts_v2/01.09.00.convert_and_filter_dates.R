convert_and_filter_dates <- function(x) {
  converted_dates <- as.Date(x, format = "%m/%d/%Y")
  # Replace dates before 1900-01-01 with NA
  converted_dates[converted_dates < as.Date("1900-01-01")] <- NA_Date_
  return(converted_dates)
}
