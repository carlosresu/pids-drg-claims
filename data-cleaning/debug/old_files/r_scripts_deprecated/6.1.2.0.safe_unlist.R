safe_unlist <- function(x) if (length(x) > 0) unlist(x, recursive = TRUE) else character(0)
