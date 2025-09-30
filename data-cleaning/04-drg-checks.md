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
