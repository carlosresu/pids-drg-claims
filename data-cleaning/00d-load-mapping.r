library(data.table)
library(here)

message("==== Loading Data Mapping ====")

# Enable caching and printing options for data mapping
to_use_cache <- TRUE # Enable saving/loading of .rds files
to_print_mapping_data <- FALSE # Set to TRUE to print mapping tables

# Helper function to load data from cache or query from BigQuery if not cached
load_or_query <- function(query, var_name, year_to_load = NULL, overwrite_cache = FALSE) {
  rds_path <- here(
    "data-cleaning/debug/cache/mapping", eclaims_batch,
    paste0(
      ifelse(var_name == "hci" & !is.null(year_to_load), paste0("hci_", year_to_load),
        ifelse(var_name == "claims" & !is.null(year_to_load), paste0("claims_", year_to_load),
          var_name
        )
      ), ".rds"
    )
  )

  if (!overwrite_cache && to_use_cache && file.exists(rds_path)) {
    message(paste("Loading from cache:", rds_path))
    return(readRDS(rds_path))
  }

  message(paste("Querying:", var_name))
  dt <- query_bq_to_dt(query)

  if (to_use_cache) saveRDS(dt, rds_path)
  return(dt)
}

# Print all rows if enabled
print_all <- function(dt, title) {
  if (to_print_mapping_data) {
    cat("\n---", title, "---\n")
    print(dt, nrow = Inf)
  }
}

# Define queries
proc_query <- paste0("SELECT * FROM ", gcp_proj, ".tdrg_libraries.proc")
proc <- load_or_query(proc_query, "proc")
proc[, CODE := as.character(CODE)]

rvs_icd9_query <- paste0(
  "SELECT * FROM ", gcp_proj,
  ".phic_rvs_crosswalk.icd9_rvs_mapping"
)
rvs_icd9 <- load_or_query(rvs_icd9_query, "rvs_icd9")

rvs_icd9 <- rvs_icd9[, .(
  rvs = as.character(rvs),
  icd9cm = as.character(as.numeric(icd9cm) * 100)
)]
rvs_icd9 <- merge(rvs_icd9, proc[, .(CODE, DRGUSE)], by.x = "icd9cm", by.y = "CODE", all.x = TRUE)
rvs_icd9 <- rvs_icd9[, is_drg := !is.na(DRGUSE) & DRGUSE][!is.na(rvs) & !is.na(icd9cm), -"DRGUSE"]

acr_rvs_query <- paste0("SELECT * FROM ", gcp_proj, ".phic_acr.procedure")
acr_rvs <- load_or_query(acr_rvs_query, "acr_rvs")

i10_query <- paste0("SELECT * FROM ", gcp_proj, ".tdrg_libraries.i10")
tdrg_icd10 <- load_or_query(i10_query, "tdrg_icd10")
setkey(tdrg_icd10, "CODE")

acc_pdx <- unique(tdrg_icd10[ACCPDX == "Y", CODE])

# Create environment for quick lookup
create_env_from_vector <- function(vec) {
  env <- new.env(parent = emptyenv())
  list2env(setNames(as.list(rep(TRUE, length(vec))), vec), envir = env)
  return(env)
}

acc_pdx_env <- create_env_from_vector(acc_pdx)

phl_icd10_query <- paste0("SELECT * FROM ", gcp_proj, ".phic_icd.phl_icd10")
phl_icd10 <- load_or_query(phl_icd10_query, "phl_icd10")

# Filter and process neoplasm codes
neoplasms_dt_actual <- as.data.table(phl_icd10[grepl("/", icd10), .(icd10)])
neoplasms_dt_actual[, icd10 := sapply(strsplit(icd10, ","), function(x) trimws(x[2]))]

i10vx_query <- paste0("SELECT * FROM ", gcp_proj, ".tdrg_libraries.i10vx")
i10vx <- load_or_query(i10vx_query, "i10vx")
setkey(i10vx, "code")

acc_icd <- unique(i10vx[, code])

zben_query <- paste0("SELECT * FROM ", gcp_proj, ".phic_acr.zben")
zben <- load_or_query(zben_query, "zben")
setkey(zben, "code")
zben[, code := gsub("[^A-Za-z0-9]", "", code)]
zben <- zben$code

acr_query <- paste0("SELECT * FROM ", gcp_proj, ".phic_acr.acr")
acr <- load_or_query(acr_query, "acr")
setkey(acr, "code")
acr[, code := gsub("[^A-Za-z0-9/\\\\]", "", code)]
acr <- acr[nchar(gsub("[^A-Za-z]", "", code)) <= 1]
acr <- acr$code

# Query HCI data. TODO: Uncomment this once hci is available for 2024 and 2025
# hci_query <- if (to_filter) {
#   paste0("SELECT * FROM ", gcp_proj, ".phic_hci.hci_full")
# } else {
#   paste0("SELECT * FROM ", gcp_proj, ".phic_hci.hci_", year_to_load)
# }
# hci <- load_or_query(hci_query, "hci", year_to_load)

# Define global variables
neoplasm_codes <- unique(neoplasms_dt_actual$icd10)
covid_codes <- unique(covid_rvs)
rvs_codes <- unique(acr_rvs$rvs)

neoplasm_pattern <- paste0("(", paste(neoplasm_codes, collapse = "|"), ")")
covid_pattern <- paste0("(", paste(covid_codes, collapse = "|"), ")")
rvs_pattern <- paste0("(", paste(rvs_codes, collapse = "|"), ")")

phil_icds <- unique(gsub("[^A-Za-z0-9]", "", phl_icd10[!grepl("/", icd10), icd10]))
icd_codes <- unique(tdrg_icd10$CODE)

# Create lookup environments
proc_env <- create_env_from_vector(proc$CODE)
rvs_env <- create_env_from_vector(rvs_icd9$rvs)
icd9cm_env <- create_env_from_vector(rvs_icd9$icd9cm)
acr_rvs_env <- create_env_from_vector(acr_rvs$rvs)
acc_pdx_env <- create_env_from_vector(acc_pdx)
phl_icd10_env <- create_env_from_vector(phl_icd10$icd10)
acc_icd_env <- create_env_from_vector(i10vx$code)
# TODO: Uncomment this once hci is available for 2024 and 2025
# hci_env <- create_env_from_vector(hci$id_hci)

neoplasm_env <- create_env_from_vector(neoplasm_codes)
covid_env <- create_env_from_vector(covid_codes)
rvs_codes_env <- create_env_from_vector(rvs_codes)
phil_icds_env <- create_env_from_vector(phil_icds)
icd_codes_env <- create_env_from_vector(icd_codes)

covid_neoplasm_codes <- unique(c(covid_codes, neoplasm_codes))
covid_neoplasm_env <- create_env_from_vector(covid_neoplasm_codes)

covid_rvs_neoplasm_codes <- unique(c(covid_codes, rvs_codes, neoplasm_codes))
covid_rvs_neoplasm_env <- create_env_from_vector(covid_rvs_neoplasm_codes)

covid_rvs_neoplasm_zben_codes <- unique(c(covid_codes, rvs_codes, neoplasm_codes, zben))
covid_rvs_neoplasm_zben_env <- create_env_from_vector(covid_rvs_neoplasm_zben_codes)

covid_rvs_neoplasm_pattern <- paste(c(covid_codes, rvs_codes, neoplasm_codes), collapse = "|")

# # Define claim queries. TODO: Uncomment this once hci is available for 2024 and 2025
# if (to_filter) {
#   claims_query <- paste0("
#   SELECT
#       c.id_series
#   FROM `pids-drg-data.phic_eclaims.eclaims_", year_to_load, "` c
#   JOIN `pids-drg-data.phic_hfac.hfac_2023` h
#       ON c.id_hci = h.id_hci
#   WHERE
#       c.claim_status = 'G'
#       AND c.is_covid = FALSE
#       AND c.clin_outpatient = FALSE
#       AND h.inst_level IN ('INF', 'L1', 'L2', 'L3');")

#   claims <- load_or_query(claims_query, "claims", year_to_load)

#   hci_filter <- hci[inst_level %chin% c("INF", "L1", "L2", "L3"), id_hci]
# }

# Prepare custom codes
custom_codes <- c(covid_rvs_neoplasm_codes, zben, acr)
custom_rvs_codes <- custom_codes[grepl("^[0-9]", custom_codes)]
custom_codes <- custom_codes[!grepl("^[0-9]", custom_codes)]
custom_codes <- unique(custom_codes)
custom_codes_sorted <- unique(custom_codes[order(-nchar(custom_codes))])

# Build ICD dictionary
icd_dict <- split_icd_by_prefix(icd_codes)
icd_dict <- extend_icd_dict_with_custom_codes(icd_dict, custom_codes_sorted)
icd_dict <- sort_icd_dict_by_length(icd_dict)
all_codes <- unlist(icd_dict, use.names = FALSE)

message("00d-load-mapping.r successfully executed.")
