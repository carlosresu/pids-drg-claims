# map_rvs_icd9 <- function(clin_rvs, rvs = rvs_icd9) {
#   # Split the RVS codes into those with and without DRG
#   with_drg <- rvs[is_drg == TRUE]
#   without_drg <- rvs[!rvs %in% with_drg$rvs]

#   # Order by RVS code and is_drg flag,
#   # group by RVS code, and list ICD-9-CM codes
#   setorder(with_drg, rvs, -is_drg)
#   unique_rvs <- with_drg[, .(icd9cm_list = list(icd9cm)), by = rvs]

#   # Separate RVS codes into solo and multi-mapped lists
#   solo <- unique_rvs[lengths(icd9cm_list) == 1]
#   list_mapped <- unique_rvs[lengths(icd9cm_list) > 1]

#   # Create named lists for solo and multi-mapped RVS codes
#   rvs_map_solo <- setNames(solo$icd9cm_list, solo$rvs)
#   rvs_map_list <- setNames(list_mapped$icd9cm_list, list_mapped$rvs)

#   # Create an environment for fast lookup of solo RVS-ICD9 mappings
#   rvs_map_solo_env <- as.environment(rvs_map_solo)

#   # Map RVS codes to ICD-9-CM codes
#   icd9_list <- lapply(clin_rvs, function(x) {
#     codes <- unlist(x)
#     mappable <- codes[!is.na(mget(
#       codes,
#       envir = rvs_map_solo_env,
#       ifnotfound = NA_character_
#     ))]
#     if (length(mappable) > 0) {
#       unique(unlist(mget(mappable, envir = rvs_map_solo_env)))
#     } else {
#       NA_character_
#     }
#   })

#   # Collect summary statistics and diagnostics for RVS mapping
#   rvss <- unique(unlist(clin_rvs))
#   mappable_rvs <- intersect(rvss, rvs$rvs)
#   unmappable_rvs <- setdiff(rvss, rvs$rvs)
#   multi_mapped_rvs <- intersect(rvss, names(rvs_map_list))
#   without_drg_codes <- unique(rvs[!rvs %in% names(rvs_map_list)]$rvs)

#   # Return the result as a list
#   return(
#     list(
#       icd9_list = icd9_list,
#       rvs_map_list = rvs_map_list,
#       rvss = rvss,
#       mappable_rvs = mappable_rvs,
#       unmappable_rvs = unmappable_rvs,
#       multi_mapped_rvs = multi_mapped_rvs,
#       without_drg = without_drg_codes
#     )
#   )
# }
# map_rvs_icd9 <- function(clin_rvs, rvs = rvs_icd9) {
#   # Split the RVS codes into those with and without DRG
#   with_drg <- rvs[is_drg == TRUE]
#   without_drg <- rvs[is_drg == FALSE]

#   # Order by RVS code and is_drg flag,
#   # group by RVS code, and list ICD-9-CM codes
#   setorder(with_drg, rvs, -is_drg)
#   unique_rvs <- with_drg[, .(icd9cm_list = list(icd9cm)), by = rvs]

#   # Separate RVS codes into solo and multi-mapped lists
#   solo <- unique_rvs[lengths(icd9cm_list) == 1]
#   list_mapped <- unique_rvs[lengths(icd9cm_list) > 1]

#   # Create named lists for solo and multi-mapped RVS codes
#   rvs_map_solo <- setNames(solo$icd9cm_list, solo$rvs)
#   rvs_map_list <- setNames(list_mapped$icd9cm_list, list_mapped$rvs)

#   # Map RVS codes to ICD-9-CM codes
#   icd9_list <- lapply(clin_rvs, function(x) {
#     codes <- unlist(x)
#     # Check if each RVS code is in the solo or multi-mapped lists
#     solo_mapped <- unlist(rvs_map_solo[codes %in% names(rvs_map_solo)])
#     multi_mapped <- unlist(rvs_map_list[codes %in% names(rvs_map_list)])
#     # Combine the mappings and remove duplicates
#     mapped_icd9 <- unique(c(solo_mapped, multi_mapped))
#     if (length(mapped_icd9) > 0) mapped_icd9 else NA_character_
#   })

#   # Collect summary statistics and diagnostics for RVS mapping
#   rvss <- unique(unlist(clin_rvs))
#   mappable_rvs <- intersect(rvss, rvs$rvs)
#   unmappable_rvs <- setdiff(rvss, rvs$rvs)
#   multi_mapped_rvs <- intersect(rvss, names(rvs_map_list))
#   without_drg_codes <- unique(rvs[!rvs %in% names(rvs_map_list)]$rvs)

#   return_list <- list(
#     icd9_list = icd9_list,
#     rvs_map_list = rvs_map_list,
#     rvss = rvss,
#     mappable_rvs = mappable_rvs,
#     unmappable_rvs = unmappable_rvs,
#     multi_mapped_rvs = multi_mapped_rvs,
#     without_drg = without_drg_codes
#   )

#   # str(return_list)

#   # Return the result as a list
#   return(return_list)
# }
map_rvs_icd9 <- function(clin_rvs, rvs = rvs_icd9) {
  # Identify RVS codes with is_drg == FALSE for later tracking
  without_drg_codes <- unique(rvs[is_drg == FALSE]$rvs)

  # Order by RVS code, group by RVS code, and list ICD-9-CM codes
  setorder(rvs, rvs)
  unique_rvs <- rvs[, .(icd9cm_list = list(icd9cm)), by = rvs]

  # Create mappings for RVS codes: solo and multi-mapped lists
  solo <- unique_rvs[lengths(icd9cm_list) == 1]
  list_mapped <- unique_rvs[lengths(icd9cm_list) > 1]

  rvs_map_solo <- setNames(solo$icd9cm_list, solo$rvs)
  rvs_map_list <- setNames(list_mapped$icd9cm_list, list_mapped$rvs)

  # Map each RVS code in clin_rvs to its specific ICD-9-CM code(s)
  icd9_list <- lapply(clin_rvs, function(x) {
    codes <- unlist(x)
    # Retrieve ICD-9-CM codes specifically for each code
    mapped_icd9 <- unique(unlist(lapply(codes, function(code) {
      if (code %in% names(rvs_map_solo)) {
        rvs_map_solo[[code]]
      } else if (code %in% names(rvs_map_list)) {
        rvs_map_list[[code]]
      } else {
        NULL
      }
    })))
    if (length(mapped_icd9) > 0) mapped_icd9 else NA_character_
  })

  # Collect summary statistics and diagnostics for RVS mapping
  rvss <- unique(unlist(clin_rvs))
  mappable_rvs <- intersect(rvss, rvs$rvs)
  unmappable_rvs <- setdiff(rvss, rvs$rvs)
  multi_mapped_rvs <- intersect(rvss, names(rvs_map_list))

  # Identify mapped codes with is_drg == FALSE
  mapped_without_drg <- intersect(mappable_rvs, without_drg_codes)

  return_list <- list(
    icd9_list = icd9_list,
    rvs_map_list = rvs_map_list,
    rvss = rvss,
    mappable_rvs = mappable_rvs,
    unmappable_rvs = unmappable_rvs,
    multi_mapped_rvs = multi_mapped_rvs,
    without_drg = mapped_without_drg
  )

  # Return the result as a list
  return(return_list)
}
