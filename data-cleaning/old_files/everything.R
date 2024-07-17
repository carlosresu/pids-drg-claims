# print_summary_statistics <- function(rvss, rvs_icd9, rvs_map_list) {
#   without_drg <- rvs_icd9[!rvs %in% names(rvs_map_list)]
#   cat(sprintf(
#     "There are %d",
#     length(unique(without_drg$rvs))
#   ), "RVS codes without an ICD-9CM equivalent recognized by the TDRG ICD9CM\n")

#   mappable_rvs <- intersect(rvss, rvs_icd9$rvs)
#   cat(sprintf(
#     "Of these, %d (%.2f%%)",
#     length(mappable_rvs), length(mappable_rvs) * 100 / length(rvss)
#   ), "have a mapping to an ICD-9-CM code.\n")

#   multi_mapped_rvs <- intersect(rvss, names(rvs_map_list))
#   cat(
#     sprintf(
#       "Of these, there are %d (%.2f%%)",
#       length(multi_mapped_rvs),
#       length(multi_mapped_rvs) * 100 / length(mappable_rvs)
#     ),
#     "with more than one ICD9 equivalent recognized by the Thai ICD9 library.\n"
#   )

#   unmappable_rvs <- setdiff(rvss, mappable_rvs)
#   cat(sprintf(
#     "There are %d (%.2f%%) with no ICD-9-CM equivalents.\n",
#     length(unmappable_rvs), length(unmappable_rvs) * 100 / length(rvss)
#   ))
# }

# collect_summary_statistics <- function(rvss, rvs_icd9, rvs_map_list) {
#   without_drg <- rvs_icd9[!rvs %in% names(rvs_map_list)]
#   without_drg_count <- length(unique(without_drg$rvs))

#   mappable_rvs <- intersect(rvss, rvs_icd9$rvs)
#   mappable_rvs_count <- length(mappable_rvs)
#   mappable_rvs_percentage <- length(mappable_rvs) * 100 / length(rvss)

#   multi_mapped_rvs <- intersect(rvss, names(rvs_map_list))
#   multi_mapped_rvs_count <- length(multi_mapped_rvs)
#   multi_mapped_rvs_percentage <- length(multi_mapped_rvs) * 100 / length(mappable_rvs)

#   unmappable_rvs <- setdiff(rvss, mappable_rvs)
#   unmappable_rvs_count <- length(unmappable_rvs)
#   unmappable_rvs_percentage <- length(unmappable_rvs) * 100 / length(rvss)

#   return(list(
#     without_drg_count = without_drg_count,
#     mappable_rvs_count = mappable_rvs_count,
#     mappable_rvs_percentage = mappable_rvs_percentage,
#     multi_mapped_rvs_count = multi_mapped_rvs_count,
#     multi_mapped_rvs_percentage = multi_mapped_rvs_percentage,
#     unmappable_rvs_count = unmappable_rvs_count,
#     unmappable_rvs_percentage = unmappable_rvs_percentage
#   ))
# }







