map_rvs_icd9 <- function(clin_rvs, rvs = rvs_icd9) {
  split_codes <- split_rvs_codes(rvs_icd9 = rvs)
  rvs_maps <- create_rvs_map_lists(split_codes$with_drg)

  rvs_map_solo_env <- as.environment(rvs_maps$rvs_map_solo)

  return(
    list(
      # main return variable (a column) to save back to dt
      icd9_list = get_icd9_codes(clin_rvs, rvs_map_solo_env),
      # other return variables that are for checks and outputs
      rvs_map_list = rvs_maps$rvs_map_list,
      rvss = unique(unlist(clin_rvs)),
      mappable_rvs = intersect(unique(unlist(clin_rvs)), rvs$rvs),
      unmappable_rvs = setdiff(unique(unlist(clin_rvs)), rvs$rvs),
      multi_mapped_rvs = intersect(
        unique(unlist(clin_rvs)),
        names(rvs_maps$rvs_map_list)
      ),
      without_drg = unique(
        rvs[!rvs %in% names(rvs_maps$rvs_map_list)]$rvs
      )
    )
  )
}
