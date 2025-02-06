source("~/drg-pipeline/data-cleaning/00a-parameters.r")
system("git submodule update --init --recursive")
required_packages <- c(
  "data.table", "here", "tictoc", "stringr", "stringi", "lubridate",
  "profvis", "hash", "future", "future.apply", "knitr", "htmlwidgets",
  "parallelly", "stringdist", "parallel", "reticulate", "bigrquery",
  "jsonlite", "googleCloudStorageR", "haven", "fst", "httr", "ggplot2",
  "rmarkdown", "digest", "base64enc", "arrow", "tidyverse"
)
github_packages <- c("r-lib/styler")
n_cores <- parallel::detectCores()
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    message("Installing ", package)
    install.packages(package, dependencies = TRUE, Ncpus = n_cores)
  } else {
    if (verbose_output) message("Loading ", package)
  }
  library(package, character.only = TRUE)
}
install_from_github <- function(repo) {
  package_name <- basename(repo)
  if (!require(package_name, character.only = TRUE)) {
    if (!require("remotes", character.only = TRUE)) {
      install.packages("remotes", Ncpus = n_cores)
    }
    message("Installing ", package_name, " from GitHub (", repo, ")")
    remotes::install_github(repo, Ncpus = n_cores)
  } else {
    if (verbose_output) message("Loading ", package_name)
  }
  library(package_name, character.only = TRUE)
}
message("Installing/loading required CRAN packages...")
invisible(
  suppressPackageStartupMessages(
    lapply(required_packages, install_and_load)
  )
)
message("Installing/loading required GitHub packages...")
invisible(
  suppressPackageStartupMessages(
    lapply(github_packages, install_from_github)
  )
)
for (file in c(
  "0.1.0.params_fpaths.R",
  "2.0.split_and_save_part.R",
  "0.3.0.summary_helper_functions.R"
)) {
  source(here::here("data-cleaning/r_scripts_v2/", file))
}
gcs_base <- "gs://phic-claims-raw/"
to_hash <- FALSE # Flag to enable or disable hashing
escape_spaces <- function(path) {
  gsub(" ", "\\\\ ", path)
}
process_file <- function(year_to_load) {
  file_type <- if (year_to_load %in% c(2022, 2023)) ".tsv" else ".csv"
  file_name <- paste0(full_claims_prefix, year_to_load, file_type)
  gcs_path <- paste0(gcs_base, file_name)
  file_path <- here::here(raw_claims_path, file_name)
  file_path_escaped <- escape_spaces(file_path)
  md5_rds_path <- here::here(raw_claims_md5_path, paste0(year_to_load, "_md5.rds"))
  if (file.exists(file_path) && (to_hash == FALSE || file.exists(md5_rds_path))) {
    if (to_debug) message(paste("File", file_name, "and its MD5 already exist. Skipping download and hash generation.\n"))
    return(NULL)
  }
  if (!file.exists(file_path)) {
    if (!is.null(gcp_proj) && gcp_proj == "drg-pipeline") {
      system(paste("gsutil cp", escape_spaces(gcs_path), file_path_escaped), intern = FALSE, ignore.stderr = FALSE)
    } else {
      stop("Error: GCP Project is not null and is not drg-pipeline")
    }
  }
  if (!to_hash) {
    if (to_debug) message(paste("Hashing disabled. Skipping MD5 operations for", file_name, "\n"))
    return(NULL)
  }
  md5_hex <- digest::digest(file(file_path, "rb"), algo = "md5", file = TRUE)
  md5_raw <- as.raw(as.numeric(strtoi(substring(md5_hex, seq(1, nchar(md5_hex), 2), seq(2, nchar(md5_hex), 2)), 16)))
  md5_base64 <- base64enc::base64encode(md5_raw)
  local_md5 <- list(md5_hex = md5_hex, md5_base64 = md5_base64)
  saveRDS(local_md5, file = md5_rds_path)
  if (to_debug) {
    cat("Generated MD5 for local file:\n")
    print(local_md5)
  }
  gcs_md5_output <- system(paste("gsutil hash", escape_spaces(gcs_path)), intern = TRUE)
  md5_line <- gcs_md5_output[grepl("Hash \\(md5\\):", gcs_md5_output)]
  if (length(md5_line) == 0) {
    stop(paste("GCS MD5 hash not found for", file_name))
  } else if (!grepl(md5_base64, md5_line)) {
    stop(paste("Mismatch detected for", file_name))
  } else {
    message(paste("MD5 match confirmed for", file_name))
  }
  rm(md5_hex, md5_raw, local_md5, gcs_md5_output, md5_line)
  gc()
}
years <- 2018:2023
for (year in years) {
  process_file(year)
}
cat("All files are up-to-date and verified.\n")
print(split_parts)
for (year_to_load in c(2018:2023)) {
  year_to_load <<- year_to_load
  invisible(source(here::here("data-cleaning/r_scripts_v2/0.1.0.params_fpaths.R")))
  full_header <- data.table::fread(
    file = full_claims_file,
    nrows = 1, colClasses = "character",
    header = TRUE
  )
  partial_file <- here::here(raw_claims_parts_path, paste0(
    full_claims_prefix, year_to_load,
    "_part_", sprintf("%02d", split_parts),
    "_of_", split_parts, ".rds"
  ))
  if (!file.exists(partial_file)) {
    full_file <- data.table::fread(
      file = full_claims_file, colClasses = "character",
      header = TRUE, encoding = "Latin-1", sep = separator
    )
  }
  parallel::mclapply(
    1:split_parts,
    split_and_save_part,
    mc.cores = nthreads / 2
  )
  print_memory_usage_gb <- function(env = .GlobalEnv) {
    obj_names <- ls(envir = env)
    obj_sizes <- sapply(obj_names, function(x) object.size(get(x, envir = env)) / (1024^3)) # Convert bytes to GB
    obj_info <- data.frame(
      Object = obj_names,
      Size_GB = round(obj_sizes, 3) # Round to 3 decimal places for readability
    )
    obj_info <- obj_info[obj_info$Size_GB > 0, ]
    obj_info <- obj_info[order(obj_info$Size_GB, decreasing = TRUE), ]
    print(obj_info, row.names = FALSE)
  }
  print_memory_usage_gb()
  if (exists("full_file")) rm(full_file)
  invisible(gc())
}
hash_cache_dir <- here::here("data-cleaning/cache/partial_md5")
dir.create(hash_cache_dir, recursive = TRUE, showWarnings = FALSE)
calculate_md5 <- function(file_path) {
  md5sum <- digest::digest(file = file_path, algo = "md5")
  return(md5sum)
}
check_md5_changes <- function(year_to_load) {
  hash_file_path <- here::here(hash_cache_dir, paste0("md5_hashes_", year_to_load, ".rds"))
  current_hashes <- sapply(1:split_parts, function(part) {
    part_file <- here::here(
      raw_claims_parts_path,
      paste0(full_claims_prefix, year_to_load, "_part_", sprintf("%02d", part), "_of_", split_parts, ".rds")
    )
    calculate_md5(part_file)
  })
  if (file.exists(hash_file_path)) {
    saved_hashes <- readRDS(hash_file_path)
    if (identical(saved_hashes, current_hashes)) {
      message(paste("No changes in partial files for year", year_to_load))
      return(TRUE)
    }
  }
  return(FALSE)
}
check_and_save_md5 <- function(year_to_load) {
  if (check_md5_changes(year_to_load)) {
    return(TRUE)
  }
  total_rows_check <- 0
  for (part in 1:split_parts) {
    part_rows <- nrow(
      readRDS(
        here::here(
          raw_claims_parts_path,
          paste0(full_claims_prefix, year_to_load, "_part_", sprintf("%02d", part), "_of_", split_parts, ".rds")
        )
      )
    )
    total_rows_check <- total_rows_check + part_rows
  }
  expected_total_rows <- readRDS(here::here("data-cleaning/cache/total_rows", paste0("total_rows_", year_to_load, ".rds")))
  if (total_rows_check == expected_total_rows) {
    message(paste("Row count matches for year", year_to_load, "- saving MD5 hashes."))
    current_hashes <- sapply(1:split_parts, function(part) {
      part_file <- here::here(
        raw_claims_parts_path,
        paste0(full_claims_prefix, year_to_load, "_part_", sprintf("%02d", part), "_of_", split_parts, ".rds")
      )
      calculate_md5(part_file)
    })
    saveRDS(current_hashes, here::here(hash_cache_dir, paste0("md5_hashes_", year_to_load, ".rds")))
    return(TRUE)
  } else {
    message(paste("Row count mismatch for year", year_to_load, "- skipping MD5 save."))
    return(FALSE)
  }
}
results <- parallel::mclapply(2018:2023, check_and_save_md5, mc.cores = nthreads)
if (all(unlist(results))) {
  message("All row counts match and MD5 hashes are updated.")
} else {
  message("Discrepancies found in row counts or updates.")
}
invisible(source(here::here("data-cleaning/r_scripts_v2/3.0.create_sample_files.R")))
for (sample_size_divisor in c(625, 125, 25, 5)) {
  sample_size_divisor <<- sample_size_divisor
  message(paste0("Starting sampling for size ÷", sample_size_divisor))
  for (year_to_load in c(2018:2023)) {
    year_to_load <<- year_to_load
    message(paste0("Generating samples of size ÷", sample_size_divisor, " for year ", year_to_load))
    invisible(source(here::here("data-cleaning/r_scripts_v2/0.1.0.params_fpaths.R")))
    parallel::mclapply(
      1:split_parts,
      function(mclapply_part) {
        sampled_claims_file <- here::here(raw_claims_samples_path, paste0(
          "sampled_claims_", year_to_load, "_", sample_size_divisor,
          "_part_", sprintf("%02d", mclapply_part), "_of_", split_parts, ".rds"
        ))
        if (!file.exists(sampled_claims_file)) {
          create_sample_files(mclapply_part, sampled_claims_file)
        }
      },
      mc.cores = nthreads
    )
    message(paste0("Done generating samples of size ÷", sample_size_divisor, " for year ", year_to_load))
  }
  message(paste0("Finished sampling for size ÷", sample_size_divisor, " for all years"))
}
source("~/drg-pipeline/data-cleaning/00a-parameters.r")
system("git submodule update --init --recursive")
required_packages <- c(
  "data.table", "here", "tictoc", "stringr", "stringi", "lubridate",
  "profvis", "hash", "future", "future.apply", "knitr", "htmlwidgets",
  "parallelly", "stringdist", "parallel", "reticulate", "bigrquery",
  "jsonlite", "googleCloudStorageR", "haven", "fst", "httr", "ggplot2",
  "rmarkdown", "digest", "base64enc", "arrow", "tidyverse"
)
github_packages <- c("r-lib/styler")
n_cores <- parallel::detectCores()
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    message("Installing ", package)
    install.packages(package, dependencies = TRUE, Ncpus = n_cores)
  } else {
    if (verbose_output) message("Loading ", package)
  }
  library(package, character.only = TRUE)
}
install_from_github <- function(repo) {
  package_name <- basename(repo)
  if (!require(package_name, character.only = TRUE)) {
    if (!require("remotes", character.only = TRUE)) {
      install.packages("remotes", Ncpus = n_cores)
    }
    message("Installing ", package_name, " from GitHub (", repo, ")")
    remotes::install_github(repo, Ncpus = n_cores)
  } else {
    if (verbose_output) message("Loading ", package_name)
  }
  library(package_name, character.only = TRUE)
}
message("Installing/loading required CRAN packages...")
invisible(
  suppressPackageStartupMessages(
    lapply(required_packages, install_and_load)
  )
)
message("Installing/loading required GitHub packages...")
invisible(
  suppressPackageStartupMessages(
    lapply(github_packages, install_from_github)
  )
)
year_to_load <- 2018
for (file in list.files(here::here("data-cleaning/r_scripts_v2"), pattern = "\\.R$", full.names = TRUE)) invisible(source(file))
message(year_to_load)
to_use_cache <- TRUE # Set to TRUE to enable saving and loading of .rds files
to_print_mapping_data <- FALSE # Set to TRUE to print mapping data tables
load_or_query <- function(query, var_name) {
  rds_path <- here(cache_path, "mapping", paste0(var_name, ".rds"))
  if (to_use_cache && file.exists(rds_path)) {
    if (verbose_output) message("Loading ", var_name, " from cache...")
    return(readRDS(rds_path))
  } else {
    if (verbose_output) message("Querying ", var_name, " from BigQuery...")
    dt <- query_bq_to_dt(query)
    saveRDS(dt, rds_path) # Save queried data to .rds cache file
    return(dt)
  }
}
if (to_print_mapping_data) {
  print_all <- function(dt, title) {
    cat("\n---", title, "---\n") # Print table title
    print(dt, nrow = Inf) # Print all rows of the data.table
  }
}
proc_query <- paste0("SELECT * FROM `", gcp_proj, ".grouper_v5.proc`")
proc <- load_or_query(proc_query, "proc")
proc[, CODE := as.character(CODE)] # Ensure the CODE column is of character type
rvs_icd9_query <- paste0("SELECT * FROM `", gcp_proj, ".phic_libraries.acr_rvs_map`")
rvs_icd9 <- load_or_query(rvs_icd9_query, "rvs_icd9")
rvs_icd9 <- rvs_icd9[, .(
  rvs = as.character(rvs),
  icd9cm = as.character(as.numeric(icd9cm) * 100)
)]
rvs_icd9 <- merge(
  rvs_icd9,
  proc[, .(CODE, DRGUSE)], # Select CODE and DRGUSE columns for merging
  by.x = "icd9cm", by.y = "CODE", all.x = TRUE
)
rvs_icd9 <- rvs_icd9[, is_drg := !is.na(DRGUSE) & DRGUSE][
  !is.na(rvs) & !is.na(icd9cm), -"DRGUSE"
]
acr_rvs_query <- paste0("SELECT * FROM `", gcp_proj, ".phic_libraries.acr_procedure`")
acr_rvs <- load_or_query(acr_rvs_query, "acr_rvs")
i10_query <- paste0("SELECT * FROM `", gcp_proj, ".grouper_v5.i10`")
tdrg_icd10 <- load_or_query(i10_query, "tdrg_icd10")
setkey(tdrg_icd10, "CODE") # Set the CODE column as key for efficient lookups
acc_pdx <- unique(tdrg_icd10[ACCPDX == "Y", CODE])
acc_pdx_env <- new.env(hash = TRUE, parent = emptyenv())
for (code in acc_pdx) {
  assign(code, TRUE, envir = acc_pdx_env)
}
phl_icd10_query <- paste0("SELECT * FROM `", gcp_proj, ".icd.phl_icd10`")
phl_icd10 <- load_or_query(phl_icd10_query, "phl_icd10")
neoplasms_dt_actual <- as.data.table(phl_icd10[
  grepl("/", icd10), .(icd10)
][, icd10 := sapply(strsplit(icd10, ","), function(x) trimws(x[2]))])
i10vx_query <- paste0("SELECT * FROM `", gcp_proj, ".grouper_v5.i10vx`")
i10vx <- load_or_query(i10vx_query, "i10vx")
setkey(i10vx, "code") # Set the code column as key for efficient lookup
acc_icd <- unique(i10vx[, code]) # Extract unique ICD codes from this table
acc_icd_set <- unique(acc_icd)
hci_query <- paste0("SELECT * FROM `", gcp_proj, ".hci.temp_hci`")
hci <- load_or_query(hci_query, "hci")
neoplasm_codes <- unique(neoplasms_dt_actual$icd10) # Unique neoplasm codes
covid_codes <- unique(covid_rvs) # Unique COVID-related codes
rvs_codes <- unique(acr_rvs$rvs) # Unique RVS codes
neoplasm_pattern <- paste0("(", paste(neoplasm_codes, collapse = "|"), ")")
covid_pattern <- paste0("(", paste(covid_codes, collapse = "|"), ")")
rvs_pattern <- paste0("(", paste(rvs_codes, collapse = "|"), ")")
phil_icds <- unique(gsub("[^A-Za-z0-9]", "", phl_icd10[!grepl("/", icd10), icd10]))
icd_codes <- unique(tdrg_icd10$CODE)
create_env_from_vector <- function(vec) {
  env <- new.env(parent = emptyenv())
  list2env(setNames(as.list(rep(TRUE, length(vec))), vec), envir = env)
  return(env)
}
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
covid_rvs_neoplasm_pattern <- paste(
  c(covid_codes, rvs_codes, neoplasm_codes),
  collapse = "|"
)
save_all_data_to_file <- function(file_path, ...) {
  args <- list(...)
  sink(file_path) # Redirect output to the specified file
  cat("\n--- All Data Tables in One View ---\n") # Header for the file
  for (name in names(args)) {
    cat("\n---", name, "---\n") # Print table name as a header within the file
    print(args[[name]], nrow = Inf, max.print = Inf)
  }
  sink() # Stop redirecting output to the file
  if (verbose_output) message("All data tables saved to ", file_path) # Confirmation message
}
output_file <- here(debug_path, "mapping_data.txt")
if (to_print_mapping_data) {
  options(max.print = 999999)
  save_all_data_to_file(
    output_file,
    grouper_v5_proc = proc,
    phic_acr_rvs_map = rvs_icd9,
    phic_acr_procedure = acr_rvs,
    grouper_v5_i10 = tdrg_icd10,
    acc_pdx = acc_pdx,
    icd_phl_icd10 = phl_icd10,
    neoplasms_dt_actual = neoplasms_dt_actual,
    grouper_v5_i10vx = i10vx,
    acc_icd = acc_icd,
    hci_temp_hci = hci
  )
  options(max.print = 1000)
}
for (loop_part in 1:split_parts) {
  start_time <- Sys.time() # Record start time for processing
  cat(paste0("\rStart reading part ", loop_part, " of ", split_parts))
  flush.console()
  read_result <- read_appropriate_file(loop_part)
  read_in_dt <- read_result$read_result_dt
  cat(paste0("\rFinished reading part ", loop_part, " of ", split_parts))
  flush.console()
  cat(paste0("\rStart chunking part ", loop_part, " of ", split_parts))
  flush.console()
  chunk_size <- ceiling(nrow(read_in_dt) / nthreads)
  chunks <- split(
    read_in_dt,
    rep(
      1:nthreads,
      each = chunk_size,
      length.out = nrow(read_in_dt)
    )
  )
  cat(paste0("\rFinished chunking part ", loop_part, " of ", split_parts))
  flush.console()
  cat(paste0("\rStart processing part ", loop_part, " of ", split_parts))
  flush.console()
  if (to_parallel) {
    parallel_results <- mclapply(
      chunks, process_chunk,
      mc.cores = nthreads
    )
  } else {
    if (!to_debug) {
      parallel_results <- lapply(chunks, process_chunk)
    } else {
      parallel_results <- list(process_chunk(chunks[[1]]))
    }
  }
  rbound_dt <- rbindlist(
    parallel_results
  )
  summarized_dt <- rbound_dt # Store the summarized data
  if (to_write) {
    saveRDS(
      summarized_dt, here(checkpoint_1_path, paste0(
        checkpoint_1_prefix, year_to_load, suffix,
        "part_", sprintf("%02d", loop_part), "_of_", split_parts, ".rds"
      )),
      compress = TRUE
    )
  }
  processing_times[[loop_part]] <- as.numeric(difftime(Sys.time(),
    start_time,
    units = "secs"
  ))
  print_status_update(loop_part, split_parts, processing_times, "clean")
  if (loop_part == 1) dim_dt <- dim(summarized_dt)
  nrow_end[[loop_part]] <- nrow(summarized_dt)
  rm(read_in_dt, rbound_dt, summarized_dt)
  invisible(gc())
}
master_dt_list <- parallel::mclapply(1:split_parts, function(read_part) {
  cat(paste("\rStarted reading part", read_part))
  flush.console()
  return_dt <- readRDS(here(checkpoint_1_path, paste0(
    checkpoint_1_prefix, year_to_load, suffix,
    "part_", sprintf("%02d", read_part), "_of_", split_parts, ".rds"
  )))
  cat(paste("\rFinished reading prt", read_part))
  flush.console()
  return(return_dt)
}, mc.cores = nthreads)
message("Commencing rbindlist")
master_dt <- rbindlist(master_dt_list, fill = TRUE)
rm(master_dt_list)
invisible(gc())
message("Finished rbindlist")
total_start_rows <- 0
total_end_rows <- 0
for (nrow_part in 1:split_parts) {
  total_start_rows <- total_start_rows + nrow_start[[nrow_part]]
  total_end_rows <- total_end_rows + nrow_end[[nrow_part]]
  if (nrow_start[[nrow_part]] != nrow_end[[nrow_part]]) {
    warning(
      "WARNING: Row Count Mismatch! Part ", nrow_part,
      " has ", nrow_start[[nrow_part]], " starting rows and ",
      nrow_end[[nrow_part]], " ending rows\n"
    )
    stop("ERROR: Row Count Mismatch")
  }
}
if (if (to_sample) total_rows / sample_size_divisor else total_rows == nrow(master_dt)) {
  message("\nRow Counts Match for All Parts and Sum to Total Rows\n")
} else {
  stop("ERROR: Total Row Count Mismatch")
}
if (to_write) {
  message("Commencing saveRDS")
  saveRDS(master_dt, here(
    checkpoint_2_path, paste0(
      checkpoint_2_prefix, year_to_load, suffix, ".rds"
    )
  ), compress = TRUE)
  message("Finished saveRDS")
}
if (exists("master_dt")) {
  message("master_dt exists, making a copy and deleting it")
  result <- data.table::copy(master_dt)
  rm(master_dt)
  invisible(gc())
  message("copied master_dt to result, deleted master_dt")
} else {
  message(paste0("master_dt doesn't exist, reading ", paste0(
    checkpoint_2_prefix, year_to_load, suffix, ".rds"
  )))
  result <- readRDS(here(
    checkpoint_2_path, paste0(
      checkpoint_2_prefix, year_to_load, suffix, ".rds"
    )
  ))
  invisible(gc())
  message(paste0("finished reading ", paste0(
    checkpoint_2_prefix, year_to_load, suffix, ".rds"
  )))
}
message(paste0("Saving ", paste0(checkpoint_2_prefix, year_to_load, suffix, "prefinal", ".rds")))
saveRDS(result, here(
  checkpoint_2_path,
  paste0(checkpoint_2_prefix, year_to_load, suffix, "prefinal", ".rds")
), compress = TRUE)
message(paste0("Finished saving ", paste0(checkpoint_2_prefix, year_to_load, suffix, "prefinal", ".rds")))
data <- readRDS("/home/resurreccion_cmc/drg-pipeline/data-cleaning/data/checkpoints/checkpoint_2_master_clean_claims/checkpoint_2_claims_2018_sampled_625_prefinal.rds")
fwrite(data, "test.csv")
if (to_post_cleaning_checks) {
  acc_pdx_set <- unique(acc_pdx)
  dt <- readRDS(here(
    checkpoint_2_path,
    paste0(checkpoint_2_prefix, year_to_load, suffix, "prefinal", ".rds")
  ))
  str(dt)
  not_in_acc_pdx <- dt$clin_pdx[!dt$clin_pdx %in% acc_pdx_set & !is.na(dt$clin_pdx)]
  all_in_acc_pdx <- length(not_in_acc_pdx) == 0
  if (all_in_acc_pdx) {
    message("All entries in dt$clin_pdx are in acc_pdx.")
  } else {
    message("Not all entries in dt$clin_pdx are in acc_pdx. Entries not in acc_pdx are:")
    print(unique(not_in_acc_pdx)) # Print unique entries not in acc_pdx
  }
  rm(all_in_acc_pdx, not_in_acc_pdx)
  invisible(gc())
}
if (to_post_cleaning_checks) {
  count_data <- dt[, .N, by = clin_pdx_source]
  setorder(count_data, clin_pdx_source)
  count_data[, clin_pdx_source := factor(clin_pdx_source, levels = c(1, 2, 3, 6, 99))]
  total_count <- sum(count_data$N)
  ggplot(count_data, aes(x = clin_pdx_source, y = N)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "black") +
    labs(title = "Histogram of clin_pdx_source", x = "clin_pdx_source", y = "Count") +
    theme_minimal() +
    scale_x_discrete(drop = FALSE) + # Ensures all categories are shown
    geom_text(aes(label = N), vjust = -0.5) + # Display count above each bar
    annotate("text", x = Inf, y = -Inf, label = paste("Total N =", total_count), hjust = 1.1, vjust = -1.5) # Display total count below
  rm(count_data, total_count)
  invisible(gc())
}
if (to_post_cleaning_checks) {
  output <- capture.output({
    cat("Structure of non-empty elements in each specified column:\n\n")
    cat("dt\n")
    str(dt)
    cat("Structure of non-empty elements in each specified column:\n\n")
    cat("c1:\n")
    str(dt[!is.na(c1) & sapply(c1, function(x) length(x) > 0 && any(nzchar(x)))]$c1)
    cat("\nc2:\n")
    str(dt[!is.na(c2) & sapply(c2, function(x) length(x) > 0 && any(nzchar(x)))]$c2)
    cat("\nclin_sdx:\n")
    str(dt[!is.na(clin_sdx) & sapply(clin_sdx, function(x) length(x) > 0 && any(nzchar(x)))]$clin_sdx)
    cat("\nclin_pdx:\n")
    str(dt[!is.na(clin_pdx) & sapply(clin_pdx, function(x) length(x) > 0 && any(nzchar(x)))]$clin_pdx)
    cat("\nclin_proc:\n")
    str(dt[!is.na(clin_proc) & sapply(clin_proc, function(x) length(x) > 0 && any(nzchar(x)))]$clin_proc)
  })
  cat(paste(output, collapse = "\n"))
  rm(output)
  invisible(gc())
}
if (to_post_cleaning_checks) {
  unique_values <- unique(unlist(dt$clin_rvs))
  unique_values <- unique_values[!is.na(unique_values) & unique_values != "NA"]
  matched_values <- unique_values[unique_values %in% rvs_icd9$rvs]
  cat(paste(matched_values, collapse = "\n"))
  rm(unique_values, matched_values)
  invisible(gc())
}
if (to_post_cleaning_checks) {
  non_empty_clin_proc_rows <- dt[!is.na(clin_proc) & sapply(clin_proc, function(x) length(x) > 0 && any(nzchar(x)))]
  print(non_empty_clin_proc_rows)
  rm(non_empty_clin_proc_rows)
  invisible(gc())
}
if (to_post_cleaning_checks) {
  result <- readRDS(here(
    checkpoint_2_path,
    paste0(checkpoint_2_prefix, year_to_load, suffix, "prefinal", ".rds")
  ))
  setkey(result, NULL) # Removes any existing key
  print(result[id_series %like% "e"])
  print(result[id_pin %like% "e"])
  print(result[id_hci %like% "e"])
  flattened_clin_sdx <- unlist(result$clin_sdx, use.names = FALSE, recursive = TRUE)
  if ("A" %chin% flattened_clin_sdx) {
    cat("Found 'A' in clin_sdx\n")
    rows_with_A <- result[sapply(result$clin_sdx, function(x) any("A" %chin% x))]
    for (i in seq_len(nrow(rows_with_A))) {
      print(rows_with_A[i])
    }
  } else {
    cat("No 'A' found in clin_sdx\n")
  }
}
if (to_post_cleaning_checks) {
  result <- readRDS(here(
    checkpoint_2_path,
    paste0(checkpoint_2_prefix, year_to_load, suffix, "prefinal", ".rds")
  ))
  date_cols <- c(
    "date_adm", "date_dis", "date_rec", "date_ref",
    "date_check", "pat_bdate", "date_ext"
  )
  rows_with_old_dates <- result[Reduce(`|`, lapply(
    .SD,
    function(x) x < as.Date("1900-01-01")
  )), .SDcols = date_cols]
  print(rows_with_old_dates)
}
result <- readRDS(here(
  checkpoint_2_path,
  paste0(checkpoint_2_prefix, year_to_load, suffix, "prefinal", ".rds")
))
print(nrow(result))
print(nrow(result))
result[, is_covid := {
  covid_found <- rep(FALSE, .N)
  not_found <- !covid_found
  covid_found[not_found] <- clin_c1[not_found] %chin% covid_rvs
  not_found <- !covid_found
  covid_found[not_found] <- clin_c2[not_found] %chin% covid_rvs
  not_found <- !covid_found
  covid_found[not_found] <- c2[not_found] %chin% covid_rvs
  not_found <- !covid_found
  covid_found[not_found] <- c1[not_found] %chin% covid_rvs
  not_found <- !covid_found
  covid_found[not_found] <- sapply(clin_rvs[not_found], function(row) any(row %chin% covid_rvs))
  not_found <- !covid_found
  covid_found[not_found] <- sapply(clin_sdx[not_found], function(row) any(row %chin% covid_rvs))
  not_found <- !covid_found
  covid_found[not_found] <- sapply(clin_proc[not_found], function(row) any(row %chin% covid_rvs))
  covid_found
}]
print(nrow(result))
saveRDS(result, here(
  checkpoint_2_path,
  paste0(checkpoint_2_prefix, year_to_load, suffix, "final", ".rds")
))
if (to_post_cleaning_checks) {
  before <- readRDS(here(
    checkpoint_2_path,
    paste0(checkpoint_2_prefix, year_to_load, suffix, ".rds")
  ))
  after <- readRDS(here(
    checkpoint_2_path,
    paste0(checkpoint_2_prefix, year_to_load, suffix, "final", ".rds")
  ))
  setkey(before, id_series)
  setkey(after, id_series)
  pat_age_diff_na <- before[after,
    on = .(id_series), nomatch = 0,
    .(id_series, pat_bdate,
      pat_age_before = x.pat_age,
      pat_age_after = i.pat_age
    ),
    by = .EACHI
  ]
  pat_age_diff_na <- pat_age_diff_na[
    (is.na(pat_age_before) & !is.na(pat_age_after)) |
      (!is.na(pat_age_before) & is.na(pat_age_after)) |
      (pat_age_before != pat_age_after)
  ]
  cat("Rows where pat_age is NA in one table but
not in the other, or where the values differ:\n")
  print(pat_age_diff_na)
  rm(before, after)
  invisible(gc())
}
message("Reading final")
result <- readRDS(here(
  checkpoint_2_path,
  paste0(checkpoint_2_prefix, year_to_load, suffix, "final", ".rds")
))
message("Finished reading final, commencing subsetting")
result <- result[, .(
  id_series, id_pin, id_hci, id_hcp, date_adm, time_adm,
  date_dis, time_dis, date_rec, date_ref, date_check, pat_type, pat_rel, pat_bdate,
  pat_age, pat_ageday, pat_sex, pat_bwt, pat_memcat_parent,
  pat_memcat_child, claim_status, claim_payout, claim_charge, is_covid,
  clin_discharge, clin_outpatient, clin_emergency, clin_acc,
  clin_c1, clin_c2, clin_sdx, clin_proc, clin_pdx, clin_pdx_source
)]
message("Finished subsetting, commencing saveRDS")
saveRDS(result, here(
  checkpoint_2_path,
  paste0(checkpoint_2_prefix, year_to_load, suffix, "final_subset_with_time", ".rds")
))
message("Finished saveRDS, commencing subsetting")
result <- result[, .(
  id_series, id_pin, id_hci, id_hcp, date_adm,
  date_dis, date_rec, date_ref, date_check, pat_type, pat_rel,
  pat_age, pat_ageday, pat_sex, pat_bwt, pat_memcat_parent,
  pat_memcat_child, claim_status, claim_payout, claim_charge, is_covid,
  clin_discharge, clin_outpatient, clin_emergency, clin_acc,
  clin_c1, clin_c2, clin_sdx, clin_proc, clin_pdx, clin_pdx_source
)]
message("Finished subsetting, commencing saveRDS")
saveRDS(result, here(
  checkpoint_2_path,
  paste0(checkpoint_2_prefix, year_to_load, suffix, "final_subset", ".rds")
))
message("Finished saveRDS")
result <- readRDS(here(
  checkpoint_2_path,
  paste0(checkpoint_2_prefix, year_to_load, suffix, "final_subset", ".rds")
))
if (to_bq) {
  if (!to_sample) bq_table <- paste0("claims_", year_to_load)
  tryCatch(
    {
      bq_table_delete(bq_table(gcp_proj, bq_dataset, bq_table))
      message("Table dropped successfully.\n")
    },
    error = function(e) {
      if (grepl("Not found", e, ignore.case = TRUE)) {
        message("Table does not exist, nothing to drop.\n")
      } else {
        stop(e)
      }
    }
  )
  tryCatch(
    {
      bq_table_create(
        bq_table(gcp_proj, bq_dataset, bq_table),
        fields = fromJSON(here(
          "data-cleaning/r_scripts_v2",
          "bq_schema_cleaning.json"
        ), simplifyDataFrame = FALSE)
      )
      message("Table created successfully.\n")
    },
    error = function(e) {
      if (grepl("already exists", e, ignore.case = TRUE)) {
        message("Table already exists. Skipping creation and upload.")
      } else {
        stop(e)
      }
    }
  )
  if (to_write) {
    chunk_size <- 250000 # Adjust the chunk size based on memory availability
    num_chunks <- ceiling(nrow(result) / chunk_size)
    for (i in seq_len(num_chunks)) {
      cat(paste("\rUploading chunk no.:", i))
      flush.console()
      chunk <- result[
        ((i - 1) * chunk_size + 1):min(i * chunk_size, nrow(result)),
      ]
      bq_table_upload(
        bq_table(gcp_proj, bq_dataset, bq_table),
        values = chunk,
        write_disposition = if (i == 1) "WRITE_EMPTY" else "WRITE_APPEND"
      )
      cat(paste("\rUploaded chunk no.:", i))
      flush.console()
    }
  }
}
source("~/drg-pipeline/data-cleaning/00a-parameters.r")
system("git submodule update --init --recursive")
required_packages <- c(
  "data.table", "here", "tictoc", "stringr", "stringi", "lubridate",
  "profvis", "hash", "future", "future.apply", "knitr", "htmlwidgets",
  "parallelly", "stringdist", "parallel", "reticulate", "bigrquery",
  "jsonlite", "googleCloudStorageR", "haven", "fst", "httr", "ggplot2",
  "rmarkdown", "digest", "base64enc", "arrow", "tidyverse"
)
github_packages <- c("r-lib/styler")
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    message("Installing ", package)
    install.packages(package, dependencies = TRUE)
  } else {
    if (verbose_output) message("Loading ", package)
  }
  library(package, character.only = TRUE)
}
install_from_github <- function(repo) {
  package_name <- basename(repo)
  if (!require(package_name, character.only = TRUE)) {
    if (!require("remotes", character.only = TRUE)) {
      install.packages("remotes")
    }
    message("Installing ", package_name, " from GitHub (", repo, ")")
    remotes::install_github(repo)
  } else {
    if (verbose_output) message("Loading ", package_name)
  }
  library(package_name, character.only = TRUE)
}
message("Installing/loading required CRAN packages...")
invisible(
  suppressPackageStartupMessages(
    lapply(required_packages, install_and_load)
  )
)
message("Installing/loading required GitHub packages...")
invisible(
  suppressPackageStartupMessages(
    lapply(github_packages, install_from_github)
  )
)
nthreads <- parallelly::availableCores()
scripts_path <- here("data-cleaning/r_scripts_v2")
r_files <- list.files(scripts_path, pattern = "\\.R$", full.names = TRUE)
for (file in r_files) {
  if (verbose_output) message(Sys.time(), " Sourcing: ", file)
  invisible(source(file))
}
message(year_to_load)
bq_dataset <- "drg_claims"
for (year in 2018:2023) {
  file_type <- if (year %in% c(2022:2023)) ".tsv" else ".csv"
  file_name <- paste0(full_claims_prefix, year, file_type)
  bq_name <- paste0(full_claims_bq_prefix, year, file_type)
  file_path <- here(raw_claims_path, file_name)
  exists <- file.exists(file_path)
  if (!exists) {
    if (!is.null(gcp_proj) && gcp_proj == "drg-pipeline") {
      system(
        paste0(
          "cd .. && gsutil cp gs://phic-claims-raw/",
          bq_name, " ", raw_claims_path
        ),
        intern = FALSE, ignore.stderr = FALSE
      )
    } else {
      stop("Error: GCP Project is not null and is not drg-pipeline")
    }
  } else {
    next
  }
}
to_use_cache <- TRUE # Set to TRUE to enable saving and loading of .rds files
to_print_mapping_data <- TRUE # Set to TRUE to print mapping data tables
load_or_query <- function(query, var_name) {
  rds_path <- here(cache_path, "mapping", paste0(var_name, ".rds"))
  if (to_use_cache && file.exists(rds_path)) {
    if (verbose_output) message("Loading ", var_name, " from cache...")
    return(readRDS(rds_path))
  } else {
    if (verbose_output) message("Querying ", var_name, " from BigQuery...")
    dt <- query_bq_to_dt(query)
    saveRDS(dt, rds_path) # Save queried data to .rds cache file
    return(dt)
  }
}
if (to_print_mapping_data) {
  print_all <- function(dt, title) {
    cat("\n---", title, "---\n") # Print table title
    print(dt, nrow = Inf) # Print all rows of the data.table
  }
}
proc_query <- paste0("SELECT * FROM `", gcp_proj, ".grouper_v5.proc`")
proc <- load_or_query(proc_query, "proc")
proc[, CODE := as.character(CODE)] # Ensure the CODE column is of character type
rvs_icd9_query <- paste0("SELECT * FROM `", gcp_proj, ".phic_libraries.acr_rvs_map`")
rvs_icd9 <- load_or_query(rvs_icd9_query, "rvs_icd9")
rvs_icd9 <- rvs_icd9[, .(
  rvs = as.character(rvs),
  icd9cm = as.character(as.numeric(icd9cm) * 100)
)]
rvs_icd9 <- merge(
  rvs_icd9,
  proc[, .(CODE, DRGUSE)], # Select CODE and DRGUSE columns for merging
  by.x = "icd9cm", by.y = "CODE", all.x = TRUE
)
rvs_icd9 <- rvs_icd9[, is_drg := !is.na(DRGUSE) & DRGUSE][
  !is.na(rvs) & !is.na(icd9cm), -"DRGUSE"
]
acr_rvs_query <- paste0("SELECT * FROM `", gcp_proj, ".phic_libraries.acr_procedure`")
acr_rvs <- load_or_query(acr_rvs_query, "acr_rvs")
i10_query <- paste0("SELECT * FROM `", gcp_proj, ".grouper_v5.i10`")
tdrg_icd10 <- load_or_query(i10_query, "tdrg_icd10")
setkey(tdrg_icd10, "CODE") # Set the CODE column as key for efficient lookups
acc_pdx <- unique(tdrg_icd10[ACCPDX == "Y", CODE])
acc_pdx_env <- new.env(hash = TRUE, parent = emptyenv())
for (code in acc_pdx) {
  assign(code, TRUE, envir = acc_pdx_env)
}
phl_icd10_query <- paste0("SELECT * FROM `", gcp_proj, ".icd.phl_icd10`")
phl_icd10 <- load_or_query(phl_icd10_query, "phl_icd10")
neoplasms_dt_actual <- as.data.table(phl_icd10[
  grepl("/", icd10), .(icd10)
][, icd10 := sapply(strsplit(icd10, ","), function(x) trimws(x[2]))])
i10vx_query <- paste0("SELECT * FROM `", gcp_proj, ".grouper_v5.i10vx`")
i10vx <- load_or_query(i10vx_query, "i10vx")
setkey(i10vx, "code") # Set the code column as key for efficient lookup
acc_icd <- unique(i10vx[, code]) # Extract unique ICD codes from this table
acc_icd_set <- unique(acc_icd)
hci_query <- paste0("SELECT * FROM `", gcp_proj, ".hci.temp_hci`")
hci <- load_or_query(hci_query, "hci")
neoplasm_codes <- unique(neoplasms_dt_actual$icd10) # Unique neoplasm codes
covid_codes <- unique(covid_rvs) # Unique COVID-related codes
rvs_codes <- unique(acr_rvs$rvs) # Unique RVS codes
neoplasm_pattern <- paste0("(", paste(neoplasm_codes, collapse = "|"), ")")
covid_pattern <- paste0("(", paste(covid_codes, collapse = "|"), ")")
rvs_pattern <- paste0("(", paste(rvs_codes, collapse = "|"), ")")
phil_icds <- unique(gsub("[^A-Za-z0-9]", "", phl_icd10[!grepl("/", icd10), icd10]))
icd_codes <- unique(tdrg_icd10$CODE)
create_env_from_vector <- function(vec) {
  env <- new.env(parent = emptyenv())
  list2env(setNames(as.list(rep(TRUE, length(vec))), vec), envir = env)
  return(env)
}
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
covid_rvs_neoplasm_pattern <- paste(
  c(covid_codes, rvs_codes, neoplasm_codes),
  collapse = "|"
)
save_all_data_to_file <- function(file_path, ...) {
  args <- list(...)
  sink(file_path) # Redirect output to the specified file
  cat("\n--- All Data Tables in One View ---\n") # Header for the file
  for (name in names(args)) {
    cat("\n---", name, "---\n") # Print table name as a header within the file
    print(args[[name]], nrow = Inf, max.print = Inf)
  }
  sink() # Stop redirecting output to the file
  if (verbose_output) message("All data tables saved to ", file_path) # Confirmation message
}
output_file <- here(debug_path, "mapping_data.txt")
if (to_print_mapping_data) {
  options(max.print = 999999)
  save_all_data_to_file(
    output_file,
    grouper_v5_proc = proc,
    phic_acr_rvs_map = rvs_icd9,
    phic_acr_procedure = acr_rvs,
    grouper_v5_i10 = tdrg_icd10,
    acc_pdx = acc_pdx,
    icd_phl_icd10 = phl_icd10,
    neoplasms_dt_actual = neoplasms_dt_actual,
    grouper_v5_i10vx = i10vx,
    acc_icd = acc_icd,
    hci_temp_hci = hci
  )
  options(max.print = 1000)
}
if (to_generate_subset && (to_python || (to_thai && to_generate_thai_txt) || to_spc)) {
  if (!to_thai_all_years) {
    cat("\rReading final\n")
    flush.console()
    result <- readRDS(here(checkpoint_2_path, paste0(checkpoint_2_prefix, year_to_load, suffix, "final_subset_with_time", ".rds")))
    print("Total Rows")
    print(nrow(result))
    result <- result[clin_outpatient == FALSE]
    print("Inpatient Rows")
    print(nrow(result))
    result <- result[is_covid == FALSE]
    print("Inpatient Non-Covid Rows")
    print(nrow(result))
    cat("\rComputing pat_bdate\n")
    result[!is.na(pat_age) & is.na(pat_bdate) & !is.na(date_adm), pat_bdate := as.Date(date_adm) - round(pat_age * 365.25)]
    result[!is.na(pat_bdate) & pat_bdate < as.Date("1900-01-01"), pat_bdate := NA_Date_]
    cat("\rComputing pat_ageday\n")
    result[, pat_ageday := NA_real_]
    result[
      !is.na(pat_age) & pat_age >= 0 & pat_age < 1 & !is.na(date_adm) & !is.na(pat_bdate) & is.na(pat_ageday),
      pat_ageday := as.integer(difftime(as.Date(format(date_adm, "%Y-%m-%d")), as.Date(pat_bdate), units = "days"))
    ]
    result[!is.na(pat_age) & pat_age >= 0 & pat_age < 1 & (pat_ageday > 365 | pat_ageday < 0), pat_ageday := 0]
    result[
      !is.na(pat_age) & pat_age >= 0 & pat_age < 1 & (pat_ageday == 365),
      `:=`(
        pat_ageday = 364, # Update pat_ageday to 364
        pat_bdate = pat_bdate + 1 # Add 1 day to pat_bdate
      )
    ]
    cat("\rFlooring pat_ageday\n")
    result[!is.na(pat_ageday), pat_ageday := as.integer(floor(pat_ageday))]
    cat("\rComputing pat_bwt\n")
    bw_dist <- c(
      round(runif(2, 0.5, 0.9), 3), # Random bwt between 0.5 and 0.9 for 2 newborns
      round(runif(8, 1.1, 1.4), 3), # Random bwt between 1.1 and 1.4 for 8 newborns
      round(runif(19, 1.6, 1.9), 3), # Random bwt between 1.6 and 1.9 for 19 newborns
      round(runif(95, 2.1, 2.4), 3), # Random bwt between 2.1 and 2.4 for 95 newborns
      round(runif(381, 2.6, 2.9), 3), # Random bwt between 2.6 and 2.9 for 381 newborns
      round(runif(375, 3.1, 3.4), 3), # Random bwt between 3.1 and 3.4 for 375 newborns
      round(runif(115, 3.5, 4.0), 3), # Random bwt between 3.5 and 4.0 for 115 newborns
      round(runif(6, 0.5, 4.0), 3) # Random bwt between 0.5 and 4.0 for 6 newborns
    )
    zero_mask <- result[, pat_age >= 0 & pat_age < 1]
    result[(is.na(pat_bwt) | pat_bwt <= 0) & zero_mask, pat_bwt := sapply(.SD$pat_bwt, function(x) sample(bw_dist, 1)), .SDcols = "pat_bwt"]
    result[!zero_mask, pat_bwt := NA_real_]
    cat("\rWriting final\n")
    flush.console()
    saveRDS(result, here(checkpoint_2_path, paste0(checkpoint_2_prefix, year_to_load, suffix, "final_subset_with_bdate_with_time", ".rds")))
  } else if (to_thai_all_years) {
    for (year_to_load in c(2018:2023)) {
      year_to_load <<- year_to_load
      year_to_load <- year_to_load
      cat("\rReading final\n")
      flush.console()
      result <- readRDS(here(checkpoint_2_path, paste0(checkpoint_2_prefix, year_to_load, suffix, "final_subset_with_time", ".rds")))
      print("Total Rows")
      print(nrow(result))
      result <- result[clin_outpatient == FALSE]
      print("Inpatient Rows")
      print(nrow(result))
      result <- result[is_covid == FALSE]
      print("Inpatient Non-Covid Rows")
      print(nrow(result))
      cat("\rComputing pat_bdate\n")
      result[!is.na(pat_age) & is.na(pat_bdate) & !is.na(date_adm), pat_bdate := as.Date(date_adm) - round(pat_age * 365.25)]
      result[!is.na(pat_bdate) & pat_bdate < as.Date("1900-01-01"), pat_bdate := NA_Date_]
      cat("\rComputing pat_ageday\n")
      result[, pat_ageday := NA_real_]
      result[
        !is.na(pat_age) & pat_age >= 0 & pat_age < 1 & !is.na(date_adm) & !is.na(pat_bdate) & is.na(pat_ageday),
        pat_ageday := as.integer(difftime(as.Date(format(date_adm, "%Y-%m-%d")), as.Date(pat_bdate), units = "days"))
      ]
      result[!is.na(pat_age) & pat_age >= 0 & pat_age < 1 & (pat_ageday > 365 | pat_ageday < 0), pat_ageday := 0]
      result[
        !is.na(pat_age) & pat_age >= 0 & pat_age < 1 & (pat_ageday == 365),
        `:=`(
          pat_ageday = 364, # Update pat_ageday to 364
          pat_bdate = pat_bdate + 1 # Add 1 day to pat_bdate
        )
      ]
      cat("\rFlooring pat_ageday\n")
      result[!is.na(pat_ageday), pat_ageday := as.integer(floor(pat_ageday))]
      cat("\rComputing pat_bwt\n")
      bw_dist <- c(
        round(runif(2, 0.5, 0.9), 3), # Random bwt between 0.5 and 0.9 for 2 newborns
        round(runif(8, 1.1, 1.4), 3), # Random bwt between 1.1 and 1.4 for 8 newborns
        round(runif(19, 1.6, 1.9), 3), # Random bwt between 1.6 and 1.9 for 19 newborns
        round(runif(95, 2.1, 2.4), 3), # Random bwt between 2.1 and 2.4 for 95 newborns
        round(runif(381, 2.6, 2.9), 3), # Random bwt between 2.6 and 2.9 for 381 newborns
        round(runif(375, 3.1, 3.4), 3), # Random bwt between 3.1 and 3.4 for 375 newborns
        round(runif(115, 3.5, 4.0), 3), # Random bwt between 3.5 and 4.0 for 115 newborns
        round(runif(6, 0.5, 4.0), 3) # Random bwt between 0.5 and 4.0 for 6 newborns
      )
      zero_mask <- result[, pat_age >= 0 & pat_age < 1]
      result[(is.na(pat_bwt) | pat_bwt <= 0) & zero_mask, pat_bwt := sapply(.SD$pat_bwt, function(x) sample(bw_dist, 1)), .SDcols = "pat_bwt"]
      result[!zero_mask, pat_bwt := NA_real_]
      cat("\rWriting final\n")
      flush.console()
      saveRDS(result, here(checkpoint_2_path, paste0(checkpoint_2_prefix, year_to_load, suffix, "final_subset_with_bdate_with_time", ".rds")))
    }
  }
  if (!to_thai_all_years && any(duplicated(result$id_series))) {
    duplicate_ids <- result$id_series[duplicated(result$id_series)]
    duplicate_rows <- result[id_series %in% duplicate_ids, ]
    cat("Rows with duplicate 'id_series':\n")
    print(duplicate_rows)
    stop("The 'id_series' column contains duplicates. Execution stopped.")
  }
  print(nrow(result))
  if (!to_thai_all_years && any(duplicated(result$caseid))) {
    duplicate_ids <- result$caseid[duplicated(result$caseid)]
    duplicate_rows <- result[caseid %in% duplicate_ids, ]
    cat("Rows with duplicate 'caseid':\n")
    print(duplicate_rows)
    stop("The 'caseid' column contains duplicates. Execution stopped.")
  }
  print(nrow(result))
}
set.seed(123)
library(ggplot2)
bw_dist <- c(
  round(runif(2, 0.5, 0.9), 3), # Random bwt between 0.5 and 0.9 for 2 newborns
  round(runif(8, 1.1, 1.4), 3), # Random bwt between 1.1 and 1.4 for 8 newborns
  round(runif(19, 1.6, 1.9), 3), # Random bwt between 1.6 and 1.9 for 19 newborns
  round(runif(95, 2.1, 2.4), 3), # Random bwt between 2.1 and 2.4 for 95 newborns
  round(runif(381, 2.6, 2.9), 3), # Random bwt between 2.6 and 2.9 for 381 newborns
  round(runif(375, 3.1, 3.4), 3), # Random bwt between 3.1 and 3.4 for 375 newborns
  round(runif(115, 3.5, 4.0), 3), # Random bwt between 3.5 and 4.0 for 115 newborns
  round(runif(6, 0.5, 4.0), 3) # Random bwt between 0.5 and 4.0 for 6 newborns
)
bw_data <- data.frame(bw_dist = bw_dist)
ggplot(bw_data, aes(x = bw_dist)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  labs(
    title = "Generated Live Filipino Infant Birthweights (Bin Width 0.5 kg)",
    x = "Newborn Birthweight (in kg)",
    y = "Number of Cases"
  ) +
  theme_minimal()
if (to_python && !to_generate_subset && to_generate_feather) {
  cat("\rReading final\n")
  flush.console()
  result <- readRDS(here(checkpoint_2_path, paste0(checkpoint_2_prefix, year_to_load, suffix, "final_subset_with_bdate_with_time", ".rds")))
}
if (to_python && to_generate_feather) {
  message("Renaming columns")
  result[, patage := as.numeric(pat_age)]
  result[, patsex := as.character(pat_sex)]
  result[, birthweight := as.numeric(pat_bwt)]
  result[, discharge := as.integer(clin_discharge)]
  result[, dob := as.Date(pat_bdate)]
  result[, ageday := as.integer(pat_ageday)]
  result[, pdx := clin_pdx]
  split_codes_from_list <- function(dt, column, prefix, max_cols) {
    split_list <- dt[[column]] # Extract the list column
    split_cols <- parallel::mclapply(
      1:max_cols,
      function(i) sapply(split_list, function(x) if (length(x) >= i) x[[i]] else NA_character_),
      mc.cores = nthreads # Automatically use all available cores
    )
    split_dt <- as.data.table(split_cols)
    setnames(split_dt, paste0(prefix, 1:max_cols))
    return(split_dt)
  }
  message("Splitting clin_sdx")
  sdx_columns <- split_codes_from_list(result, "clin_sdx", "sdx", 12)
  message("Splitting clin_proc")
  proc_columns <- split_codes_from_list(result, "clin_proc", "proc", 20)
  message("cbind results")
  result <- cbind(result, sdx_columns, proc_columns)
  for_fwrite <- result[, c(
    "id_series", "date_adm", "date_dis", "time_adm", "time_dis", "patage", "dob", "patsex", "discharge", "pdx",
    paste0("sdx", 1:12), paste0("proc", 1:20), "birthweight", "ageday"
  ), with = FALSE]
  message("Formatting date_adm")
  for_fwrite[, date_adm := format(date_adm, "%Y-%m-%d %H:%M:%S")]
  message("Formatting date_dis")
  for_fwrite[, date_dis := format(date_dis, "%Y-%m-%d %H:%M:%S")]
  for_fwrite[, time_adm := NULL]
  for_fwrite[, time_dis := NULL]
  message("Writing to csv")
  fwrite(for_fwrite, here(checkpoint_7_path, paste0(checkpoint_7b_prefix, suffix, ".csv")))
  message("Creating summary table")
  summary_table <- for_fwrite[, lapply(.SD, function(x) sum(!is.na(x))), .SDcols = names(for_fwrite)]
  summary_table <- transpose(summary_table)
  setnames(summary_table, "Non-Null Count")
  summary_table[, Column := names(for_fwrite)]
  setcolorder(summary_table, c("Column", "Non-Null Count"))
  message("Printing summary table")
  print(summary_table)
  saveRDS(for_fwrite, here(checkpoint_7_path, paste0("for_fwrite_", year_to_load, suffix, ".rds")))
}
if (to_python) {
  if (!to_generate_py_fwrite && to_generate_feather) for_fwrite <- readRDS(here(checkpoint_7_path, paste0("for_fwrite_", year_to_load, suffix, ".rds")))
  if (to_generate_feather) write_feather(as.data.frame(for_fwrite), here(checkpoint_7_path, paste0("python_input_", year_to_load, suffix, ".feather")))
  if (to_py_prompt) {
    response <- tolower(readline(prompt = "Have you run the Python grouper manually? (y/n): "))
    if (response != "y") {
      stop("Python Grouper not run yet. Script terminated. Continue on manually if necessary")
    }
    message("Continuing with the script...\n")
  } else {
    message("Python Grouper is assumed to have been run already. Continuing with the script...\n")
  }
  output_dt <- as.data.table(read_feather(here(checkpoint_8_path, paste0("python_output_", year_to_load, suffix, ".feather"))))
}
if (to_python) {
  setnames(output_dt,
    old = c("drg", "pdc", "pccl", "error_code", "warning_code"),
    new = c("py_drg", "py_pdc", "py_pccl", "py_err", "py_warn"), skip_absent = TRUE
  )
  required_columns <- c("id_series", "py_drg", "py_pdc", "py_pccl", "py_err", "py_warn")
  output_dt <- output_dt[, ..required_columns]
  output_dt[, py_drg := as.character(py_drg)]
  output_dt[, py_pdc := as.character(py_pdc)]
  output_dt[, py_pccl := as.numeric(py_pccl)]
  array_columns <- c("py_err", "py_warn")
  process_error_warning_column <- function(col) {
    lapply(col, function(x) {
      x <- unlist(x)
      x <- as.character(x)
      if (is.null(x) || length(x) == 0) {
        return(character(0))
      }
      x <- x[!is.na(x)]
      x <- x[!(x %in% c("None", "NA", "NaN", ""))]
      if (length(x) == 0) {
        return(character(0))
      }
      split_x <- unlist(strsplit(x, ",\\s*"))
      split_x <- split_x[!(split_x %in% c("", "NaN", "NA", "None")) & !is.na(split_x)]
      if (length(split_x) == 0) {
        return(character(0))
      } else {
        return(split_x)
      }
    })
  }
  output_dt[, (array_columns) := mclapply(.SD, process_error_warning_column, mc.cores = nthreads), .SDcols = array_columns]
  output_dt[, py_drg := ifelse(is.na(py_drg), "", py_drg)]
  output_dt[, py_pdc := ifelse(is.na(py_pdc), "", py_pdc)]
  print(head(output_dt, 100))
}
if (to_python) {
  if (to_debug) fwrite(output_dt, "test3.csv")
}
if (to_python && any(duplicated(output_dt$id_series))) {
  stop("The 'id_series' column contains duplicates. Execution stopped.")
}
if (to_python && to_py_bq) {
  bq_table <- if (nrow(output_dt) == nrow(result)) {
    paste0("python_", year_to_load)
  } else {
    paste0("temp_python_", year_to_load)
  }
  tryCatch(
    {
      bq_table_delete(bq_table(gcp_proj, bq_dataset, bq_table))
      message("Table dropped successfully.\n")
    },
    error = function(e) {
      if (grepl("Not found", e, ignore.case = TRUE)) {
        message("Table does not exist, nothing to drop.\n")
      } else {
        stop(e)
      }
    }
  )
  tryCatch(
    {
      bq_table_create(
        bq_table(gcp_proj, bq_dataset, bq_table),
        fields = fromJSON(here(
          "data-cleaning/r_scripts_v2",
          "bq_schema_thai.json"
        ), simplifyDataFrame = FALSE)
      )
      message("Table created successfully.\n")
    },
    error = function(e) {
      if (grepl("already exists", e, ignore.case = TRUE)) {
        message("Table already exists. Skipping creation and upload.")
      } else {
        stop(e)
      }
    }
  )
  if (to_write) {
    chunk_size <- 250000 # Adjust the chunk size based on memory availability
    num_chunks <- ceiling(nrow(output_dt) / chunk_size)
    for (i in seq_len(num_chunks)) {
      cat(paste("\rUploading chunk no.:", i))
      flush.console()
      chunk <- output_dt[
        ((i - 1) * chunk_size + 1):min(i * chunk_size, nrow(output_dt)),
      ]
      bq_table_upload(
        bq_table(gcp_proj, bq_dataset, bq_table),
        values = chunk,
        write_disposition = if (i == 1) "WRITE_EMPTY" else "WRITE_APPEND"
      )
      cat(paste("\rFinished uploading chunk no.:", i))
      flush.console()
    }
  }
}
if (to_thai) {
  if (!to_thai_all_years) {
    if (to_generate_thai_txt) {
      if (to_spc) {
        cat("\rReading stata\n")
        flush.console()
        result <- readRDS(here(checkpoint_2_path, paste0(checkpoint_2_prefix, year_to_load, suffix, "stata_subset_with_bdate", ".rds")))
      } else {
        cat("\rReading final\n")
        flush.console()
        result <- readRDS(here(checkpoint_2_path, paste0(checkpoint_2_prefix, year_to_load, suffix, "final_subset_with_bdate_with_time", ".rds")))
      }
      result[, caseid := as.character(seq_len(nrow(result)))]
      result_mapping <- result[, .(id_series, caseid)]
      cat("\rExporting for grouper\n")
      flush.console()
      chunk_size <- 5000000
      num_chunks <- ceiling(nrow(result) / chunk_size)
      for (i in seq_len(num_chunks)) {
        output_file <- here(
          checkpoint_4_path,
          paste0(
            checkpoint_4_prefix, year_to_load, suffix,
            "part_", i, "_of_", num_chunks, ".txt"
          )
        )
        start_row <- (i - 1) * chunk_size + 1
        end_row <- min(i * chunk_size, nrow(result))
        chunk <- result[start_row:end_row, ]
        export_for_grouper(chunk, output_file, i)
        message("Saved part ", i, " of ", num_chunks, " to ", output_file)
        message("Uploading part ", i, " of ", num_chunks, " to GCS")
        gcs_upload(
          file = output_file,
          bucket = gcs_bucket,
          name = paste0(gcs_pre_fpath, "/", basename(output_file)),
          predefinedAcl = "bucketLevel"
        )
        rm(chunk)
        gc()
      }
    } else {
      message("Skipping thai txt generation")
    }
  } else if (to_thai_all_years) {
    if (to_generate_thai_txt) {
      for (year_to_load in c(2018:2023)) {
        year_to_load <<- year_to_load
        year_to_load <- year_to_load
        cat("\rReading final\n")
        flush.console()
        result <- readRDS(here(checkpoint_2_path, paste0(checkpoint_2_prefix, year_to_load, suffix, "final_subset_with_bdate_with_time", ".rds")))
        result[, caseid := as.character(seq_len(nrow(result)))]
        result_mapping <- result[, .(id_series, caseid)]
        cat("\rExporting for grouper\n")
        flush.console()
        chunk_size <- 5000000
        num_chunks <- ceiling(nrow(result) / chunk_size)
        for (i in seq_len(num_chunks)) {
          output_file <- here(
            checkpoint_4_path,
            paste0(
              checkpoint_4_prefix, year_to_load, suffix,
              "part_", i, "_of_", num_chunks, ".txt"
            )
          )
          start_row <- (i - 1) * chunk_size + 1
          end_row <- min(i * chunk_size, nrow(result))
          chunk <- result[start_row:end_row, ]
          export_for_grouper(chunk, output_file, i)
          message("Saved part ", i, " of ", num_chunks, " to ", output_file)
          message("Uploading part ", i, " of ", num_chunks, " to GCS")
          gcs_upload(
            file = output_file,
            bucket = gcs_bucket,
            name = paste0(gcs_pre_fpath, "/", basename(output_file)),
            predefinedAcl = "bucketLevel"
          )
          rm(chunk)
          gc()
        }
      }
    } else {
      message("Skipping thai txt generation")
    }
  }
  if (to_thai_prompt && to_generate_thai_txt && !to_thai_all_years) {
    response <- tolower(readline(prompt = "Have you run the Thai grouper manually? (y/n): "))
    if (response != "y") {
      stop("Thai Grouper not run yet. Script terminated. Continue on manually if necessary")
    }
    message("Continuing with the script...\n")
  } else {
    message("Thai Grouper is assumed to have been run already. Continuing with the script...\n")
  }
  if (!to_thai_all_years) {
    cat("\rDownloading Grouper results\n")
    flush.console()
    cat("\rReading final\n")
    flush.console()
    result <- readRDS(here(checkpoint_2_path, paste0(checkpoint_2_prefix, year_to_load, suffix, "final_subset_with_bdate_with_time", ".rds")))
    result[, caseid := as.character(seq_len(nrow(result)))]
    result_mapping <- result[, .(id_series, caseid)]
    chunk_size <- 5000000
    num_chunks <- ceiling(nrow(result) / chunk_size)
    thai_result <- list()
    for (i in seq_len(num_chunks)) {
      remote_file <- paste0(
        gcs_post_fpath,
        "/",
        toupper(
          paste0(
            checkpoint_5_prefix, year_to_load, suffix,
            "part_", i, "_of_", num_chunks
          )
        ),
        "Res.TXT"
      )
      local_file <- here(
        checkpoint_5_path,
        paste0(
          toupper(
            paste0(
              checkpoint_5_prefix, year_to_load, suffix,
              "part_", i, "_of_", num_chunks
            )
          ),
          "Res.TXT"
        )
      )
      message("Downloading part ", i, " of ", num_chunks, " from GCS")
      gcs_get_object(
        object_name = remote_file,
        bucket = gcs_bucket,
        saveToDisk = local_file,
        overwrite = TRUE
      )
      part_data <- fread(local_file, colClasses = "character")
      thai_result[[i]] <- part_data
      rm(part_data)
      gc()
    }
    thai_result <- rbindlist(thai_result, use.names = FALSE, fill = FALSE)
    cat(paste("nrow thai_result:", nrow(thai_result), "\n"))
    cat(paste("nrow result_mapping:", nrow(result_mapping), "\n"))
    cat(paste("nrow result:", nrow(result), "\n"))
    message("All parts downloaded and combined successfully.\n")
    if (to_debug) print(head(thai_result))
    if (any(duplicated(thai_result$caseid))) {
      duplicate_ids <- thai_result$caseid[duplicated(thai_result$caseid)]
      duplicate_rows <- thai_result[caseid %in% duplicate_ids, ]
      cat("Rows with duplicate 'id_series':\n")
      print(duplicate_rows)
      stop("The 'id_series' column contains duplicates. Execution stopped.")
    }
    thai_result <- merge(
      thai_result,
      result_mapping, # Select only caseid and id_series from result_mapping
      by = "caseid", # Column to join on
      all.x = TRUE,
      all.y = FALSE,
    )
    cat(paste("nrow thai_result:", nrow(thai_result), "\n"))
    cat(paste("nrow result_mapping:", nrow(result_mapping), "\n"))
    cat(paste("nrow result:", nrow(result), "\n"))
    cat("\rRenaming columns\n")
    flush.console()
    thai_result[, row := caseid]
    thai_result[, caseid := id_series]
    thai_result[, id_series := NULL]
    thai_result[, thai_drg := drg]
    thai_result[, thai_rw := rw]
    thai_result[, thai_wtlos := wtlos]
    thai_result[, thai_ot := ot]
    thai_result[, thai_adjrw := adjrw]
    thai_result[, thai_err := err]
    thai_result[, thai_warn := warn]
    thai_result[, thai_los := los]
    thai_result[, drg := NULL]
    thai_result[, drgname := NULL]
    thai_result[, rw := NULL]
    thai_result[, wtlos := NULL]
    thai_result[, ot := NULL]
    thai_result[, adjrw := NULL]
    thai_result[, err := NULL]
    thai_result[, warn := NULL]
    thai_result[, los := NULL]
  }
}
if (to_thai && !to_thai_all_years) {
  print(nrow(thai_result))
  print(nrow(thai_result[thai_err == "6"]))
}
if (to_python && to_thai && !to_thai_all_years) {
  if (exists("output_dt")) {
    before_merge <- data.table::copy(output_dt)
    before_merge[, caseid := id_series]
    if (to_debug) print(head(before_merge))
    merged <- merge(before_merge, thai_result, by = "caseid", all.x = TRUE)
    if (to_debug) print(head(merged))
    diff_merged <- merged[!as.character(ifelse(is.na(py_drg), "NA", py_drg)) == as.character(thai_drg)]
    print(nrow(diff_merged))
    fwrite(diff_merged, here(checkpoint_9_path, paste0("checkpoint_9_grouper_differences_", year_to_load, suffix, ".csv")))
  }
}
if (to_python && to_thai && !to_thai_all_years) {
  if (exists("merged")) { # str(merged)
    if (to_debug) fwrite(merged, paste0(year_to_load, "test4.csv"))
  }
}
if (to_thai && !to_thai_all_years) {
  result_after_thai <- data.table::copy(thai_result)
  result_after_thai[, id_series := caseid]
  result_after_thai[, caseid := NULL]
  result_after_thai[, thai_drg := as.character(thai_drg)]
  result_after_thai[, thai_rw := as.numeric(thai_rw)]
  result_after_thai[, thai_wtlos := as.numeric(thai_wtlos)]
  result_after_thai[, thai_ot := as.integer(thai_ot)]
  result_after_thai[, thai_adjrw := as.numeric(thai_adjrw)]
  result_after_thai[, thai_err := as.integer(thai_err)]
  result_after_thai[, thai_warn := as.integer(thai_warn)]
  result_after_thai[, thai_los := as.integer(thai_los)]
  setcolorder(result_after_thai, c(
    "row",
    "id_series",
    "thai_drg",
    "thai_rw",
    "thai_wtlos",
    "thai_ot",
    "thai_adjrw",
    "thai_err",
    "thai_warn",
    "thai_los"
  ))
}
if (to_thai && !to_thai_all_years) {
  print(result_after_thai[grepl("e", id_series)])
}
if (to_thai && !to_thai_all_years) {
}
if (to_thai && !to_thai_all_years) {
  if (any(duplicated(result_after_thai$id_series))) {
    duplicate_ids <- result_after_thai$id_series[duplicated(result_after_thai$id_series)]
    duplicate_rows <- result_after_thai[id_series %in% duplicate_ids, ]
    cat("Rows with duplicate 'id_series':\n")
    print(duplicate_rows)
    stop("The 'id_series' column contains duplicates. Execution stopped.")
  }
}
print(nrow(result))
if (to_thai && !to_thai_all_years) print(nrow(result_after_thai))
if (to_thai && !to_thai_all_years) print(result_after_thai[is.na(thai_drg)])
if (to_thai && !to_thai_all_years) print(result_after_thai[is.na(id_series)])
if (to_thai && !to_thai_all_years && to_thai_bq) {
  result_after_thai[, row := NULL]
  saveRDS(result_after_thai, here(checkpoint_6_path, paste0(checkpoint_6_prefix, year_to_load, suffix, ".rds")))
  prefix <- if (!to_sample) "thai_" else "temp_thai_"
  bq_table <- paste0(prefix, year_to_load)
  if (!to_spc) {
    tryCatch(
      {
        bq_table_delete(bq_table(gcp_proj, bq_dataset, bq_table))
        message("Table dropped successfully.\n")
      },
      error = function(e) {
        if (grepl("Not found", e, ignore.case = TRUE)) {
          message("Table does not exist, nothing to drop.\n")
        } else {
          stop(e)
        }
      }
    )
    tryCatch(
      {
        bq_table_create(
          bq_table(gcp_proj, bq_dataset, bq_table),
          fields = fromJSON(here(
            "data-cleaning/r_scripts_v2",
            "bq_schema_thai.json"
          ), simplifyDataFrame = FALSE)
        )
        message("Table created successfully.\n")
      },
      error = function(e) {
        if (grepl("already exists", e, ignore.case = TRUE)) {
          message("Table already exists. Skipping creation and upload.")
        } else {
          stop(e)
        }
      }
    )
    if (to_write) {
      chunk_size <- 250000 # Adjust the chunk size based on memory availability
      num_chunks <- ceiling(nrow(result_after_thai) / chunk_size)
      for (i in seq_len(num_chunks)) {
        cat(paste("\rUploading chunk no.:", i))
        flush.console()
        chunk <- result_after_thai[
          ((i - 1) * chunk_size + 1):min(i * chunk_size, nrow(result_after_thai)),
        ]
        bq_table_upload(
          bq_table(gcp_proj, bq_dataset, bq_table),
          values = chunk,
          write_disposition = if (i == 1) "WRITE_EMPTY" else "WRITE_APPEND"
        )
        cat(paste("\rUploaded chunk no.:", i))
        flush.console()
      }
    }
  } else {
    message("Skipping BigQuery upload as to_spc is TRUE.")
  }
}
