# Thai DRG Grouper (v2) — Batch Execution Guide

This guide explains how to run the Thai DRG Grouper using `03a-drg-grouping-thai-v2.ps1` on Windows with Sandboxie, AutoHotkey, and GCS integration.

---

## ✅ Prerequisites

Before running the script, ensure the following are installed and configured:

### Required Software

- **AutoHotkey v2**

  - Installed **for all users**
  - Ensure `C:\Program Files\AutoHotkey\v2\AutoHotkey64.exe` exists.

- **Sandboxie Plus**

  - Installed **for all users**
  - Should include:
    - `C:\Program Files\Sandboxie-Plus\Start.exe`
    - `C:\Program Files\Sandboxie-Plus\SbieCtrl.exe`

- **Google Cloud SDK (gcloud CLI)**
  - Installed and authenticated using `gcloud init`
  - User must have access to `gs://pids-drg-data`

### Repo & Files

- Repository cloned locally on a **Windows computer** (e.g. `C:\Users\resur\Documents\pids-drg-claims`)
- Run the following notebook beforehand **on the VM with raw claims** to generate `.txt` files:
  ```
  ~/pids-drg-claims/data-cleaning/03-drg-grouping-v2.ipynb
  ```
- The Thai Grouper binaries (`TGRP50V02.exe` and supporting files) must be copied to:
  ```
  <repo root>\TDRGv5\
  ```

---

## ▶️ How to Run

1. **Open PowerShell (Run as Administrator)**
2. Change to the `data-cleaning` directory:
   ```powershell
   cd "$((Resolve-Path .\).Path)\pids-drg-claims\data-cleaning"
   ```
3. Execute the script:
   ```powershell
   ./03a-drg-grouping-thai-v2.ps1
   ```

---

## 🧩 Prompt Guide

During execution, the script will prompt you for the following:

| Prompt                                                | Description                                                                                                                     |
| ----------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------- |
| `Enable debugFlag? [y/n]`                             | Enables debug mode. If set to `y`, only files ending in `*of_1.txt` will be downloaded from GCS for testing.                    |
| `Delete old input files in '<chkpt4>'? [y/n]`         | If `y`, clears previous input `.txt` files before proceeding.                                                                   |
| `Delete old output files in '<chkpt5>'? [y/n]`        | If `y`, clears previously processed output `.TXT` files.                                                                        |
| `Download files from gs://.../pre to <chkpt4>? [y/n]` | If `y`, fetches raw input `.txt` files from the GCS bucket to local input folder. Skips if you’ve already placed them manually. |

---

## 📁 Folder Structure

| Folder                       | Description                                                                    |
| ---------------------------- | ------------------------------------------------------------------------------ |
| `chkpt_4_thai_master_input`  | Local folder containing `.txt` input files to process.                         |
| `chkpt_5_thai_master_output` | Output directory where processed `*Res.TXT` files will be moved before upload. |

---

## 🛠️ What the Script Does

1. **Kills any running AutoHotkey or TGRP50V02 processes** to avoid conflicts.
2. **Cleans up** previous sandbox definitions from `Sandboxie.ini`.
3. **Creates dynamic sandbox configurations** (BoxAA, BoxAB, etc.).
4. **Launches each input file** using the Thai DRG Grouper in its own sandbox.
5. **AutoHotkey v2** script (`ignore_numeric_overflows.ahk`) runs in the background to dismiss popups.
6. **Waits until all sandboxed processes** are completed.
7. **Moves output files** (`*Res.TXT`) to the output checkpoint folder.
8. **Validates completeness** of all grouped outputs.
9. **Uploads results to GCS** at `gs://pids-drg-vm/data/thai/post`.

---

## 🧪 Debug Mode

When `debugFlag` is enabled:

- Only files matching `*of_1.txt` will be downloaded
- Useful for small-scale test runs

---

## 🆘 Troubleshooting

- **Script won’t run**? Ensure PowerShell is running as Administrator.
- **`Start-Process` fails**? Check your AutoHotkey and Sandboxie paths.
- **No output files?** Confirm the `.txt` files are valid and all parts are present (e.g., `part_1_of_2`, `part_2_of_2`, etc.).
- **GCS access denied?** Run `gcloud auth login` and ensure access to `pids-drg-data`.

---

## 📦 Output Upload

Final results are uploaded automatically to:

```
gs://pids-drg-vm/data/thai/post/
```

Make sure your gcloud CLI is authenticated and initialized correctly.

---

## ✅ Done!

Once all steps complete and output is uploaded, you should see:

```
All steps completed.
```

Happy processing!
