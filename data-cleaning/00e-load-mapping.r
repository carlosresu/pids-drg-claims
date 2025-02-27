library(data.table)
library(here)

message("==== Loading Data Mapping ====")

# Enable caching and printing options for data mapping
to_use_cache <- TRUE # Enable saving/loading of .rds files
to_print_mapping_data <- FALSE # Set to TRUE to print mapping tables

# Helper function to load data from cache or query from BigQuery if not cached
load_or_query <- function(query, var_name, year = NULL, overwrite_cache = FALSE) {
  rds_path <- here("data-cleaning/debug/cache/mapping",
    paste0(
      ifelse(var_name == "hci" & !is.null(year), paste0("hci_", year),
        ifelse(var_name == "claims" & !is.null(year), paste0("claims_", year),
          var_name)
      ), ".rds")
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
proc_query <- paste0("SELECT * FROM ", gcp_proj, ".grouper_v5.proc")
proc <- load_or_query(proc_query, "proc")
proc[, CODE := as.character(CODE)]

rvs_icd9_query <- paste0("SELECT * FROM ", gcp_proj, ".phic_libraries.acr_rvs_map")
rvs_icd9 <- load_or_query(rvs_icd9_query, "rvs_icd9")

rvs_icd9 <- rvs_icd9[, .(
  rvs = as.character(rvs),
  icd9cm = as.character(as.numeric(icd9cm) * 100)
)]
rvs_icd9 <- merge(rvs_icd9, proc[, .(CODE, DRGUSE)], by.x = "icd9cm", by.y = "CODE", all.x = TRUE)
rvs_icd9 <- rvs_icd9[, is_drg := !is.na(DRGUSE) & DRGUSE][!is.na(rvs) & !is.na(icd9cm), -"DRGUSE"]

acr_rvs_query <- paste0("SELECT * FROM ", gcp_proj, ".phic_libraries.acr_procedure")
acr_rvs <- load_or_query(acr_rvs_query, "acr_rvs")

i10_query <- paste0("SELECT * FROM ", gcp_proj, ".grouper_v5.i10")
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

phl_icd10_query <- paste0("SELECT * FROM ", gcp_proj, ".icd.phl_icd10")
phl_icd10 <- load_or_query(phl_icd10_query, "phl_icd10")

# Filter and process neoplasm codes
neoplasms_dt_actual <- as.data.table(phl_icd10[grepl("/", icd10), .(icd10)])
neoplasms_dt_actual[, icd10 := sapply(strsplit(icd10, ","), function(x) trimws(x[2]))]

i10vx_query <- paste0("SELECT * FROM ", gcp_proj, ".grouper_v5.i10vx")
i10vx <- load_or_query(i10vx_query, "i10vx")
setkey(i10vx, "code")

acc_icd <- unique(i10vx[, code])

# Query HCI data
hci_query <- if (to_filter) {
  paste0("SELECT * FROM ", gcp_proj, ".phic_hci.hci_full")
} else {
  paste0("SELECT * FROM ", gcp_proj, ".phic_hci.hci_", year)
}
hci <- load_or_query(hci_query, "hci", year)

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
hci_env <- create_env_from_vector(hci$id_hci)

neoplasm_env <- create_env_from_vector(neoplasm_codes)
covid_env <- create_env_from_vector(covid_codes)
rvs_codes_env <- create_env_from_vector(rvs_codes)
phil_icds_env <- create_env_from_vector(phil_icds)
icd_codes_env <- create_env_from_vector(icd_codes)

covid_neoplasm_codes <- unique(c(covid_codes, neoplasm_codes))
covid_neoplasm_env <- create_env_from_vector(covid_neoplasm_codes)

covid_rvs_neoplasm_codes <- unique(c(covid_codes, rvs_codes, neoplasm_codes))
covid_rvs_neoplasm_env <- create_env_from_vector(covid_rvs_neoplasm_codes)

covid_rvs_neoplasm_pattern <- paste(c(covid_codes, rvs_codes, neoplasm_codes), collapse = "|")

# Define claim queries
if (to_filter) {
  claims_query <- paste0("
  SELECT
      c.id_series
  FROM `drg-pipeline.phic_claims.claims_", year, "` c
  JOIN `drg-pipeline.phic_hci.hci_full` h
      ON c.id_hci = h.id_hci
  WHERE
      c.claim_status = 'G'
      AND c.is_covid = FALSE
      AND c.clin_outpatient = FALSE
      AND h.inst_level IN ('INF', 'L1', 'L2', 'L3');")

  claims <- load_or_query(claims_query, "claims", year)

  hci_filter <- hci[inst_level %chin% c("INF", "L1", "L2", "L3"), id_hci]
}

message("00f-load-mapping.r successfully executed.")
