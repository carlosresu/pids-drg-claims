# 01.02.01 Convert time to period (using lubridate)
convert_to_hms <- function(seconds) {
  # Round seconds to the nearest whole number
  period <- seconds_to_period(round(seconds))
  return(period)
}
