# map_icd10 <- function(c1, c2, clin_icd, thai_icd10 = tdrg_icd10, covidrvs = covid_rvs) {
#   # Helper: Get unique ICD codes from input columns
#   get_unique_icd_codes <- function(c1, c2, clin_icd) {
#     icds <- unique(c(unlist(c1), unlist(c2), unlist(clin_icd)))
#     icds <- icds[!is.na(icds)]
#     return(icds)
#   }

#   # Helper: Generate ICD10 mapping
#   generate_icd10_mapping <- function(icds, thai_codes, neoplasm_codes, covidrvs) {
#     icd_mapping <- list()
#     modified_count <- 0

#     for (d in icds) {
#       d <- str_trim(d)

#       if (d %in% covidrvs) next

#       if (d %in% thai_codes) {
#         icd_mapping[[d]] <- d
#       } else if (!(d %in% neoplasm_codes) && grepl("[A-Za-z]", d) && grepl("[0-9]", d)) {
#         if (nchar(d) == 3 && paste0(d, "9") %in% thai_codes) {
#           icd_mapping[[d]] <- paste0(d, "9")
#           modified_count <- modified_count + 1
#         } else if (nchar(d) >= 4) {
#           for (i in seq_len(nchar(d) - 3)) {
#             new_d <- substr(d, 1, nchar(d) - i)
#             if (new_d %in% thai_codes) {
#               icd_mapping[[d]] <- new_d
#               modified_count <- modified_count + 1
#               break
#             }
#           }
#         }
#       }
#     }

#     return(list(icd_mapping_res = icd_mapping, modified_count = modified_count))
#   }

#   # Helper: Apply ICD10 mapping to columns
#   apply_icd10_mapping <- function(codes, icd_mapping) {
#     sapply(codes, function(code) if (code %in% names(icd_mapping)) icd_mapping[[code]] else code)
#   }

#   # Step 1: Get unique ICD codes
#   icds <- get_unique_icd_codes(c1, c2, clin_icd)

#   # Step 2: Extract Thai ICD10 and neoplasm codes
#   thai_codes <- unique(thai_icd10$CODE)
#   neoplasm_codes <- unique(thai_icd10[grepl("/", thai_icd10$CODE), "CODE"])

#   # Step 3: Generate ICD10 mapping
#   icd_mapping_info <- generate_icd10_mapping(icds, thai_codes, neoplasm_codes, covidrvs)
#   icd_mapping <- icd_mapping_info$icd_mapping_res
#   modified_count <- icd_mapping_info$modified_count

#   # Step 4: Identify unmatched ICD codes
#   unmatched_icds <- setdiff(icds, names(icd_mapping))

#   # Step 5: Create data.table for unmatched codes and their sources
#   unmatched_sources <- data.table(code = unmatched_icds, source = NA_character_, count = 0)

#   for (col_name in c("c1", "c2", "clin_icd")) {
#     col_values <- get(col_name)
#     unmatched_sources[code %in% unlist(col_values), source := col_name]
#     unmatched_sources[code %in% unlist(col_values), count := count + table(unlist(col_values))[code]]
#   }

#   unmatched_sources <- unmatched_sources[order(-count)]

#   # Step 6: Create data.table for ICD10 mapping
#   icd10_map <- data.table(phl_icd10 = names(icd_mapping), thai_icd10 = unlist(icd_mapping))

#   # Step 7: Apply ICD10 mapping to columns
#   c1_mapped <- lapply(c1, apply_icd10_mapping, icd_mapping)
#   c2_mapped <- lapply(c2, apply_icd10_mapping, icd_mapping)
#   clin_icd_mapped <- lapply(clin_icd, apply_icd10_mapping, icd_mapping)

#   # Step 8: Return results
#   return(list(
#     c1 = c1_mapped,
#     c2 = c2_mapped,
#     clin_icd = clin_icd_mapped,
#     icd10_map_dt = icd10_map,
#     unique_icds = icds,
#     direct_matches = names(icd_mapping),
#     unmatched = unmatched_icds,
#     unmatched_sources = unmatched_sources,
#     icd_mapping_res = icd_mapping
#   ))
# }

map_icd10 <- function(c1, c2, clin_icd, thai_icd10 = tdrg_icd10, covidrvs = covid_rvs) {
  # Helper: Create named lists for environments
  create_named_list <- function(codes) {
    if (length(codes) > 0) {
      setNames(as.list(rep(TRUE, length(codes))), codes)
    } else {
      list()
    }
  }

  # Step 1: Create environments for Thai codes and neoplasm codes
  thai_env <- new.env(hash = TRUE)
  neoplasm_env <- new.env(hash = TRUE)

  # Step 2: Populate environments with named lists
  list2env(create_named_list(thai_icd10$CODE), envir = thai_env)
  neoplasm_codes <- thai_icd10[grepl("/", thai_icd10$CODE), "CODE"]
  list2env(create_named_list(neoplasm_codes), envir = neoplasm_env)

  # Step 3: Helper to get unique ICD codes
  get_unique_icd_codes <- function(...) {
    unique(unlist(list(...), use.names = FALSE))
  }

  # Step 4: Generate ICD10 mapping using environment lookups with batch processing
  generate_icd10_mapping <- function(icds, covidrvs) {
    icd_mapping <- new.env(hash = TRUE)
    modified_count <- 0

    # Filter out invalid and COVID-related codes
    valid_icds <- setdiff(icds[!is.na(icds) & icds != ""], covidrvs)

    # Batch lookup using mget for faster performance
    thai_matches <- mget(valid_icds, envir = thai_env, ifnotfound = list(NULL))
    neoplasm_matches <- mget(valid_icds, envir = neoplasm_env, ifnotfound = list(NULL))

    for (d in valid_icds) {
      d <- str_trim(d)
      if (!is.null(thai_matches[[d]])) {
        icd_mapping[[d]] <- d
      } else if (is.null(neoplasm_matches[[d]]) && grepl("[A-Za-z]", d) && grepl("[0-9]", d)) {
        if (nchar(d) == 3 && !is.null(thai_matches[[paste0(d, "9")]])) {
          icd_mapping[[d]] <- paste0(d, "9")
          modified_count <- modified_count + 1
        } else if (nchar(d) >= 4) {
          # Substring search for longer codes
          for (i in seq_len(nchar(d) - 3)) {
            new_d <- substr(d, 1, nchar(d) - i)
            if (!is.null(thai_matches[[new_d]])) {
              icd_mapping[[d]] <- new_d
              modified_count <- modified_count + 1
              break
            }
          }
        }
      }
    }
    list(icd_mapping_res = as.list(icd_mapping), modified_count = modified_count)
  }

  # Step 5: Apply mappings using vectorization
  apply_icd10_mapping <- function(codes, icd_mapping) {
    vapply(codes, function(code) icd_mapping[[code]] %||% code, character(1))
  }

  # Step 6: Get unique ICD codes
  icds <- get_unique_icd_codes(c1, c2, clin_icd)

  # Step 7: Generate ICD10 mapping
  icd_mapping_info <- generate_icd10_mapping(icds, covidrvs)
  icd_mapping <- icd_mapping_info$icd_mapping_res

  # Step 8: Identify unmatched ICD codes
  unmatched_icds <- setdiff(icds, names(icd_mapping))

  # Step 9: Create data.table for unmatched codes
  unmatched_sources <- rbindlist(lapply(c("c1", "c2", "clin_icd"), function(col_name) {
    col_values <- get(col_name)
    data.table(code = unlist(col_values), source = col_name)[, .(count = .N), by = .(code, source)]
  }))
  unmatched_sources <- unmatched_sources[code %in% unmatched_icds, ][order(-count)]

  # Step 10: Create data.table for ICD10 mapping
  icd10_map <- data.table(phl_icd10 = names(icd_mapping), thai_icd10 = unlist(icd_mapping))

  # Step 11: Apply ICD10 mapping to input columns
  c1_mapped <- lapply(c1, apply_icd10_mapping, icd_mapping)
  c2_mapped <- lapply(c2, apply_icd10_mapping, icd_mapping)
  clin_icd_mapped <- lapply(clin_icd, apply_icd10_mapping, icd_mapping)

  # Step 12: Return results
  list(
    c1 = c1_mapped,
    c2 = c2_mapped,
    clin_icd = clin_icd_mapped,
    icd10_map_dt = icd10_map,
    unique_icds = icds,
    direct_matches = names(icd_mapping),
    unmatched = unmatched_icds,
    unmatched_sources = unmatched_sources,
    icd_mapping_res = icd_mapping
  )
}
