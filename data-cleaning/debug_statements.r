# Inspect ICD Inputs
cat("\n==== ICD INPUTS ====\n")
str(icd_inputs)

# Inspect ICD Outputs
cat("\n==== ICD OUTPUTS ====\n")
str(icd_outputs)

# Inspect RVS Inputs
cat("\n==== RVS INPUTS ====\n")
str(rvs_inputs)

# Inspect RVS Outputs
cat("\n==== RVS OUTPUTS ====\n")
str(rvs_outputs)

# Inspect unique lengths for debugging
cat("\n==== UNIQUE LENGTHS ====\n")
cat("icd_inputs$c1:", length(icd_inputs$c1), "\n")
cat("icd_inputs$c2:", length(icd_inputs$c2), "\n")
cat("icd_inputs$clin_icd:", length(icd_inputs$clin_icd), "\n")
cat("icd_outputs$c1:", length(icd_outputs$c1), "\n")
cat("icd_outputs$c2:", length(icd_outputs$c2), "\n")
cat("icd_outputs$clin_sdx:", length(icd_outputs$clin_sdx), "\n")
cat("rvs_inputs$clin_rvs:", length(rvs_inputs$clin_rvs), "\n")
cat("rvs_outputs$clin_proc:", length(rvs_outputs$clin_proc), "\n")

# Check if list columns have matching lengths
cat("\n==== LIST COLUMN LENGTH CHECK ====\n")
cat("All icd_inputs$c1 lengths: ", lengths(icd_inputs$c1), "\n")
cat("All icd_inputs$c2 lengths: ", lengths(icd_inputs$c2), "\n")
cat("All icd_inputs$clin_icd lengths: ", lengths(icd_inputs$clin_icd), "\n")
cat("All icd_outputs$c1 lengths: ", lengths(icd_outputs$c1), "\n")
cat("All icd_outputs$c2 lengths: ", lengths(icd_outputs$c2), "\n")
cat("All icd_outputs$clin_sdx lengths: ", lengths(icd_outputs$clin_sdx), "\n")
cat("All rvs_inputs$clin_rvs lengths: ", lengths(rvs_inputs$clin_rvs), "\n")
cat("All rvs_outputs$clin_proc lengths: ", lengths(rvs_outputs$clin_proc), "\n")

# Check for NA values in the lists
cat("\n==== NA CHECK ====\n")
cat("NA count in icd_inputs$c1:", sum(is.na(unlist(icd_inputs$c1))), "\n")
cat("NA count in icd_inputs$c2:", sum(is.na(unlist(icd_inputs$c2))), "\n")
cat("NA count in icd_inputs$clin_icd:", sum(is.na(unlist(icd_inputs$clin_icd))), "\n")
cat("NA count in icd_outputs$c1:", sum(is.na(unlist(icd_outputs$c1))), "\n")
cat("NA count in icd_outputs$c2:", sum(is.na(unlist(icd_outputs$c2))), "\n")
cat("NA count in icd_outputs$clin_sdx:", sum(is.na(unlist(icd_outputs$clin_sdx))), "\n")
cat("NA count in rvs_inputs$clin_rvs:", sum(is.na(unlist(rvs_inputs$clin_rvs))), "\n")
cat("NA count in rvs_outputs$clin_proc:", sum(is.na(unlist(rvs_outputs$clin_proc))), "\n")
