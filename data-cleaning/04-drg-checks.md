---
title: "DRG Checks & Post-Pipeline Validation Guide"
author: "Carlos Resurreccion"
date: "2025-04-03"
---

# Post-Pipeline Validation — Comprehensive Documentation

This guide documents the **post-pipeline validation checks** that verify the cleaned outputs produced by the **DRG Cleaning v2** process. The validations compare **only** the original input and the final cleaned outputs—**no** imputation or grouping recomputation occurs here.

---

## ✅ Checklist

### 1. Overall Checks

- [x] Column & Schema Integrity (renaming, presence, order)
- [x] Row Count Consistency (raw vs partials vs final)
- [x] Identifier Format (no scientific notation)
- [x] Categorical Mapping Coherence (patient type, membership, discharge, claim status)

### 2. Grouping Columns & Value Validations

- [x] PDx (Primary Diagnosis) Acceptance
- [x] Patient Age Plausibility
- [x] Sex & Discharge Enumeration Validity
- [ ] DRG Grouping Validation (placeholder; not executed here)

### 3. Mapping Checks (ICD & RVS Codes)

- [x] ICD Replacement Consolidation (coverage and anomalies)
- [x] RVS → ICD-9-CM Mapping Consolidation
- [x] Date Lower Bound Validity (≥ 1900-01-01)

---

## What this notebook does (TL;DR)

This notebook runs **post-pipeline validation** on DRG Cleaning v2 outputs by comparing:

- **Input**: Raw eClaims (`dt_raw`)
- **Outputs**: Final cleaned subset for BQ (`dt_clean_bq_subset`), full cleaned file (`dt_clean_full`)

It verifies:

1. **Schema** (columns, order, presence)
2. **Row counts** (raw vs partials vs final)
3. **Identifier formats** (no scientific notation)
4. **Categorical mappings** (membership, type, sex, discharge, status)
5. **Value sanity** (PDx set, age plausibility, dates ≥ 1900-01-01)
6. **Mapping quality** (ICD & RVS mapping coherence; summary stats & exports)

### Quickstart

1. Set `year_to_load` and paths in **00a-parameters.r**.
2. Run **Setup → Libraries → R Scripts → Load Mapping Data**.
3. Execute **Load Files (Lengthy)** to populate `dt_raw`, `dt_clean_bq_subset`, `dt_clean_full`.
4. Run **Data Verification Proper** from Test Batch 01 → 08.
5. (Optional) Run **Test Batch 09** to consolidate mapping artifacts.

---

## Conventions & Execution Model

- **Check variables** follow the pattern:  
  `chk_<section>_<nn>_<suffix> = c(<flag>, <info>)`

  - `flag` is a logical pass/fail.
  - `info` is `NULL` on pass, or a human-readable diagnostic string on fail.

- **Section aggregator**:  
  `test_checks(<section>)` scans the calling frame for `chk_<section>_*` variables and:

  - **Stops** on any failure with a single consolidated error:
    ```
    ❌ Validation failed in section <section>:
    • <suffix>: <details>
    • <suffix>: <details>
    ```
  - **Prints** a compact pass summary when all checks succeed.

- **Helper utilities**:

  - `custom_setdiff(x, y)` → returns `c(TRUE, NULL)` if `setequal`; else `c(FALSE, "<missing> / <invalid>")`.
  - `custom_setequal(x, y)` → returns `c(TRUE, NULL)` if identical; else `c(FALSE, "X: <x> Y: <y>")`.
  - `custom_check_mappings(result_list)` → collapses per-column mapping diagnostics, keeping only failures.

- **Debugging**:
  - Set `to_debug = TRUE` to stream intermediate diagnostics for mapping checks.
  - Use `str()` and `uniqueN()` for quick offender introspection.
  - Ensure all date columns are `Date` class for reliable comparisons.

---

## Inputs & Dependencies

- **Tables**

  - `dt_raw`: Raw input after column selection/renaming (character-safe read).
  - `dt_clean_bq_subset`: Final BigQuery subset used for schema validation.
  - `dt_clean_full`: Full cleaned file (includes dates and age for plausibility checks).

- **Parameters / Lookups**

  - `bq_cols`, `column_mappings`, `expected_mappings`, `acc_pdx`
  - `split_parts`, `raw_claims_parts_path`, `eclaims_batch`
  - BigQuery schema JSON: `bq_schema_cleaning.json` or `bq_schema_cleaning_2025.json`

- **Utilities**
  - MD5 cache of partial files to avoid unnecessary heavy IO.

---

## Test Batches

### Test Batch 01 — Column & Schema Integrity

**Purpose**  
Verify that `dt_clean_bq_subset` has **exactly** the expected columns and matches the BigQuery schema for the current `eclaims_batch`.

**Checks**

- `chk_01_01_cols_match_expected` — `dt_clean_bq_subset` vs `bq_cols`
- `chk_01_02_cols_match_schema` — `dt_clean_bq_subset` vs BQ schema JSON

**Criteria**

- **Pass**: Both diffs empty (no `missing` or `invalid`).
- **Fail**: Pretty-printed lists of `missing:` and `invalid:` columns.

**Remediation**

- Align `column_mappings`, regenerate the subset, or update schema JSON for new releases.
- Note: Load-job ordering is enforced by BQ; here we assert presence/absence.

---

### Test Batch 02 — Row Count Consistency

**Purpose**  
Detect truncation or duplication between raw input, partials, and final outputs.

**Checks**

- `chk_02_01_nrows_match_full` — `nrow(dt_raw)` vs `nrow(dt_clean_bq_subset)`
- `chk_02_02_nrows_match_partial` — If partial **MD5 changed**, sum `nrow(part_i)` via `read_appropriate_file()` and compare to `nrow(dt_raw)`; otherwise short-circuit.

**Criteria**

- **Pass**: Exact equality (or partials short-circuited as unchanged).
- **Fail**: Prints `X: <expected> Y: <actual>`.

**Remediation**

- Rebuild altered partials; verify `split_parts`, file paths, and that no unintended filter was applied during finalization.

**Performance**

- MD5 short-circuit saves unnecessary IO across large years.

---

### Test Batch 03 — Identifier Format (No Scientific Notation)

**Purpose**  
Guard against coercion that renders IDs like `1.23e+12`.

**Checks**

- Grep `"e"` (case-insensitive) in `id_series`, `id_pin`, `id_hci`.

**Criteria**

- **Pass**: Zero offenders for each ID field.
- **Fail**: Emit associated `id_series` plus the corrupted field for review.

**Remediation**

- Upstream: `colClasses = "character"` on read.
- Exports: Use `formatC()`/string formatting to preserve identifiers.

---

### Test Batch 04 — Categorical Mapping Coherence

**Purpose**  
Ensure deterministic mapping from **raw value → cleaned value** for categorical columns.

**Mechanics**

- For each `col_name` in `expected_mappings`:
  - Collect `id_series` where `dt_raw[[col_name]] == raw_val`.
  - Pull corresponding cleaned values from `dt_clean_bq_subset[[col_name]]`.
  - **Rule**: For any raw value, the cleaned set must be `{NA}` or a **singleton**.

**Check**

- `chk_04_01_mapping_results` via `custom_check_mappings()`.

**Criteria**

- **Pass**: No raw value maps to multiple cleaned values.
- **Fail**: Structured listing per column

**Remediation**

- Update lookup tables; normalize case/whitespace; patch explicit exceptions.

**Debugging**

- Set `to_debug = TRUE` to print raw→ID set→clean exemplars.

---

### Test Batch 05 — Accepted PDx

**Purpose**  
Ensure each non-null `clin_pdx` belongs to `acc_pdx`.

**Check**

- `chk_05_01_no_unacceptable_pdx`.

**Criteria**

- **Pass**: Zero rows where `clin_pdx ∉ acc_pdx` (ignoring NA).
- **Fail**: Output `id_series` and offending PDx.

**Remediation**

- Update `acc_pdx` for legitimate additions; normalize punctuation/case in inputs.

---

### Test Batch 06 — Age Plausibility

**Purpose**  
Validate that `pat_age` is consistent with `pat_bdate` and `date_adm`, and within plausible bounds.

**Checks**

- Recompute age as `floor((date_adm - pat_bdate) / 365.25)` and compare to `pat_age`.
- Range check: `0 ≤ pat_age ≤ 124`.

**Criteria**

- **Pass**: Zero offenders across both checks.
- **Fail**: Emit `id_series`, `date_adm`, `pat_bdate`, `pat_age`.

**Remediation**

- Re-parse dates; correct birthdates; recompute age upstream if needed.

**Notes**

- Using 365.25 days aligns with operational rounding.

---

### Test Batch 07 — Sex & Discharge Enumeration Validity

**Purpose**  
Enforce enumerations for patient sex and discharge disposition.

**Checks**

- `pat_sex ∈ {"M","F"}` and non-missing.
- `clin_discharge ∈ {NA, 1, 2, 3, 4, 9}`.

**Criteria**

- **Pass**: Zero invalids.
- **Fail**: Emit `id_series` and invalid values.

**Remediation**

- Extend enumerations if policy changed; fix mapping tables to match current codebooks.

---

### Test Batch 08 — Date Lower Bound Validity

**Purpose**  
Reject implausible pre-modern dates.

**Checks**

- Each of `date_adm`, `date_dis`, `date_rec`, `date_ref`, `date_check`, `pat_bdate`, `date_ext` must be `≥ 1900-01-01`.

**Criteria**

- **Pass**: Zero offenders per field.
- **Fail**: Field-specific tables with `id_series` and invalid dates.

**Remediation**

- Correct parsing (e.g., month/day swaps), time zone issues, placeholder dates (e.g., `0001-01-01`), or missing centuries.

---

### Test Batch 09 — Mapping Artifacts (ICD & RVS) — Consolidation

**Purpose**  
Consolidate **ICD** and **RVS→ICD-9-CM** mappings generated during the pipeline for QA and reuse.

**Process**

- Identify latest timestamped partial `.rds` files by `year` and `suffix` (sampled/full).
- Bind all parts, deduplicate by `raw_code`, keep the **most frequent** `mapped_code` with counts.
- Compute coverage and **unmappable** statistics.

**Outputs**

- `summary_stats_<year>_<suffix>_<ts>.csv` — counts and unique ratios of unmappable vs total, per code system.
- `icd_<year>_<suffix>_<ts>.csv`, `rvs_<year>_<suffix>_<ts>.csv` — long lists with `{raw_code, mapped_code, count}`.
- `final_map_<year>_<suffix>_<ts>.rds` — list containing `final_icd_dt`, `final_rvs_dt`.
- Original partial `.rds` moved to `_rds_files/<year>/<suffix>/<ts>/`.

**Policy**

- Informational by default. Consider thresholds to **fail** when `unmappable_percent` exceeds tolerance.

**Remediation**

- Improve normalizers and regex cleaning; enrich lookup tables; investigate high-frequency unmappable tails.

**Safety Switch (optional)**

- Gate this batch with an option to avoid heavy IO on large years:
- `options(drg.run_mapping_exports = TRUE)` to enable; otherwise skip.

---

## Interpreting Failure Messages

- **Header**:  
  `❌ Validation failed in section <NN>` indicates the failing batch.
- **Bullets**:  
  `• <suffix>: <details>`
- **Schema**: shows `missing:` and/or `invalid:` lists
- **Mapping**: `"$ "<raw>": MAPPING FAILED"` → non-singleton cleaned set
- **Counts**: `X:` (expected) vs `Y:` (actual)

**Next steps**  
Slice by `id_series` in `dt_clean_*` to reproduce and triage. Update lookups, parsing rules, or upstream transforms accordingly.

---

## Operational Notes

- **Reproducibility**: Partial-file MD5 caching prevents silent drift between runs.
- **Performance**: Prefer `data.table` joins and vectorized operations during diagnostics.
- **Extensibility**: Add/modify enumerations by extending `expected_mappings` and accepted sets.
- **Error Budgeting**: Wrap `test_checks()` calls in `tryCatch()` during exploratory runs to continue subsequent batches.

---

## Appendix — Field Reference

**Identifiers**  
`id_series`, `id_pin`, `id_hci`

**Dates**  
`date_adm`, `date_dis`, `date_rec`, `date_ref`, `date_check`, `pat_bdate`, `date_ext`

**Demographics**  
`pat_sex`, `pat_age`, `pat_type`, `pat_memcat_parent`, `pat_memcat_child`

**Clinical**  
`clin_pdx`, `clin_discharge`, `clin_rvs`

**Lookup / Config**  
`bq_cols`, `column_mappings`, `expected_mappings`, `acc_pdx`, `eclaims_batch`, `split_parts`

**Artifacts (Batch 09)**  
`final_icd_dt(raw_code, mapped_code, count)`,  
`final_rvs_dt(raw_code, mapped_code, count)`,  
`summary_stats_*`

---

## Maintenance & Governance

**Ownership**

- Primary maintainer: DRG Cleaning v2 team
- Point of contact: Project lead / data pipeline architect

**Versioning**

- This document applies to **DRG Cleaning v2** post-pipeline validation as of 2025-04-03.
- Schema updates (e.g., new BQ schema JSON files) require updating **Test Batch 01** expectations.
- New categorical enumerations require updating **expected_mappings** for **Test Batch 04** and **Test Batch 07**.

**Auditability**

- All failures produce reproducible subsets keyed by `id_series`.
- Export offenders to CSV when working with external stakeholders.
- MD5 checks on partials ensure repeatable lineage between raw splits and consolidated outputs.

**Extending Checks**

- Add a new test batch by:
  1. Defining `chk_<NN>_<nn>_<suffix> = c(flag, info)` variables.
  2. Calling `test_checks(NN)` at the end of the batch.
  3. Documenting the batch purpose, pass/fail criteria, and remediation steps here.

---

## Future Improvements

- **DRG Grouping Validation** (placeholder in Test Batch 02):

  - Add a batch that asserts presence and plausibility of DRG grouping codes post-classification.
  - Define acceptable sets, thresholds for missingness, and cross-check with ICD/RVS inputs.

- **Automated Thresholds**:

  - Currently, all failures are binary (stop/fail).
  - Consider tolerance thresholds (e.g., ≤0.1% unmappable codes allowed) before failing.

- **Reporting Enhancements**:

  - Integrate with RMarkdown or Quarto to output a **QA Validation Report** (HTML/PDF).
  - Include summary stats, histograms of offenders, and trend over years.

- **Continuous Integration (CI)**:
  - Wire these checks into a GitHub Action or scheduled job.
  - Fail builds if schema drifts or critical mismatches are detected.

---

## Key Takeaways

- Validation here is **post-hoc**: it ensures the **outputs** match expectations, but does not alter the data.
- **Failures** always point to either:
  1. Schema drift (upstream changes not reflected in mappings).
  2. Data quality issues (corruption, invalid codes, unrealistic values).
- **Pass** across all batches means the data is **structurally sound** and **ready** for downstream loading and analytics.

---
