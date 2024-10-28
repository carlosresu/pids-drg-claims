get_icd9_codes <- function(clin_rvs, rvs_map_solo_env) {
  ## Maps a column containing RVS codes to ICD-9-CM

  # Use lapply to loop through each row of clin_rvs
  lapply(clin_rvs, function(x) {
    codes <- unlist(x) # Unlist the RVS codes in each row

    # Map all codes to ICD-9-CM equivalents using the rvs_map_solo_env environment
    mappable <- codes[!is.na(mget(codes, envir = rvs_map_solo_env, ifnotfound = NA_character_))]

    if (length(mappable) > 0) {
      # Return the unique set of mapped ICD-9 codes
      unique(unlist(mget(mappable, envir = rvs_map_solo_env)))
    } else {
      NA_character_ # Return NA if no mappable codes are found
    }
  })
}
