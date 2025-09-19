# pids-drg-claims

# End-to-End Walkthrough

This guide consolidates everything you need to **stand up the VM**, **prepare partials**, **run the cleaning pipeline**, **perform DRG grouping**, and **run checks** for the repository **pids-drg-claims** (shadow-billing branch). It mirrors the flow you outlined and matches the level of beginner-friendliness used in your other repos.

> **Who is this for?**  
> New contributors who can follow step-by-step instructions to run the notebooks in **VS Code** (via **Remote Tunnels**) on a **GCP VM**. No prior Linux experience is required; copy-paste commands exactly as shown.

---

## 0) Repo Layout (for orientation)

> You downloaded an attached ZIP in this walkthrough session. Here’s a trimmed view so you can orient yourself before cloning/using the live repo.

```
[pids-drg-claims-shadow-billing]
  <dir> data-cleaning
  <dir> miscellaneous
  <dir> vm-setup
  README.md
  data-cleaning
  miscellaneous
  vm-setup
[pids-drg-claims-shadow-billing/data-cleaning]
  00a-parameters.r
  00b-packages.r
  00c-load-params-and-scripts.r
  00d-load-mapping.r
  01-drg-partial.ipynb
  02-drg-cleaning-v3.ipynb
  03-drg-grouping-v2.ipynb
  03a-drg-grouping-thai-v2.ps1
  03b-drg-grouping-py-v2.ipynb
  03c-drg-grouping-bq-v2.ipynb
  04-drg-checks.ipynb
  libraries
  misc
  requirements.txt
  scripts
  tests
[pids-drg-claims-shadow-billing/data-cleaning/r_scripts_v2]
  0.0.table_of_contents.txt
  0.1.0.params_fpaths.R
  0.2.0.process_helper_functions.R
  0.3.0.grouping_functions.R
  1.0.query_bq_to_dt.R
  2.0.split_and_save_part.R
  3.0.create_sample_files.R
  4.0.read_appropriate_file.R
  5.1.0.staged_declumping_functions.R
  5.2.0.append_copy_remove_icd_rvs_c1_c2.R
  5.3.0.swap_icd_rvs.R
  5.4.0.remap_patient_data.R
  5.5.0.map_rvs_icd9.R
  5.6.0.map_icd10.R
  5.7.0.find_pdx.R
  bq_schema_cleaning.json
  bq_schema_cleaning_2025.json
  bq_schema_python.json
  bq_schema_spc.json
  bq_schema_thai.json
  bq_schema_thai_bwt.json
```

For day-to-day work, you’ll **clone from GitHub** on your VM (instructions below). Treat this ZIP as a snapshot for reference only.

---

## 1) VM Creation and Configuration (GCP)

This VM is configured **only** for running Python and R code, via **gcloud SSH** or **Jupyter notebooks through VS Code Remote Tunnels**.

- **Included:** Jupyter, pyenv, R + IRkernel, shared data folder `/home/data`
- **Excluded:** Chrome Remote Desktop, .NET, second-disk setup, patch-job/maintenance automations.

### 1.1 Create the VM (Cloud Shell)

Open **GCP Cloud Shell** and run:

```bash
gcloud compute instances create pids-drg-claims-v2 \
  --project=pids-drg-data \
  --zone=us-central1-a \
  --machine-type=n2d-highmem-16 \
--network-interface=network-tier=PREMIUM,stack-type=IPV4_ONLY,subnet=default \
  --metadata=enable-osconfig=TRUE,enable-oslogin=true \
  --can-ip-forward \
  --maintenance-policy=MIGRATE \
  --provisioning-model=STANDARD \
  --service-account=10962838043-compute@developer.gserviceaccount.com \
  --scopes=https://www.googleapis.com/auth/cloud-platform \
  --min-cpu-platform=AMD\ Milan \
  --tags=http-server,https-server,lb-health-check \
--create-disk=auto-delete=yes,boot=yes,device-name=pids-drg-claims-boot-disk-v2,image=projects/ubuntu-os-cloud/global/images/ubuntu-minimal-2404-noble-amd64-v20250828,mode=rw,size=300,type=pd-standard \
  --shielded-secure-boot \
  --shielded-vtpm \
  --shielded-integrity-monitoring \
  --labels=goog-ec-src=vm_add-gcloud \
  --reservation-affinity=any \
  --deletion-protection
```

**Notes**

- Deletion protection is **ON** by default. Disable manually before deleting the VM.
- The `lb-health-check` tag is harmless if unused.

Enable **OS Config / VM Manager** (run once per project):

```bash
gcloud compute os-config project-feature-settings update   --project pids-drg-data   --patch-and-config-feature-set=full

gcloud compute os-config project-feature-settings describe   --project pids-drg-data
```

### 1.2 System-wide configuration (SSH into the VM)

SSH to the VM (from Cloud Shell or the console) and run the following.

#### A) Basic system prep

```bash
# Parallel builds for source-compiled packages
echo 'export MAKEFLAGS="-j$(nproc)"' | sudo tee -a /etc/environment
source /etc/environment

# Update base OS
sudo apt update
sudo apt upgrade -y
```

#### B) Data-science packages for Python & R

```bash
# Data science essentials for Python & R
# (Numerics, build toolchains, compression, plotting -- no GIS)
# -------------------------------

# Core build tools & utilities
sudo apt install -y \
  build-essential \
  wget curl git \
  pkg-config \
  ncdu dstat procps

# Python runtime + Jupyter
sudo apt install -y \
  python3-venv python3-dev python3-full python3-pip \
  jupyter jupyter-core jupyter-client \
  pandoc npm

# Numerics: fast BLAS/LAPACK (NumPy/Scipy/R)
sudo apt install -y \
  libopenblas-dev \
  liblapack-dev \
  libgfortran5

# Compression & Python build headers (for pyenv builds / C extensions)
sudo apt install -y \
  zlib1g-dev \
  libbz2-dev \
  libreadline-dev \
  libsqlite3-dev \
  libncursesw5-dev \
  xz-utils \
  tk-dev \
  libffi-dev \
  liblzma-dev \
  libxml2-dev \
  libxmlsec1-dev

# R package build deps (HTTP, SSL, crypto, git bindings)
sudo apt install -y \
  libcurl4-openssl-dev \
  libssl-dev \
  libsodium-dev \
  libgit2-dev

# Plotting & text rendering stack for R/matplotlib (no GIS)
sudo apt install -y \
  libcairo2-dev \
  libpng-dev \
  libjpeg-dev \
  libtiff5-dev \
  libfreetype6-dev \
  libfontconfig1-dev \
  libharfbuzz-dev \
  libfribidi-dev \
  libglib2.0-dev

# Optional: common fonts for nicer plots
sudo apt install -y \
  fonts-dejavu fonts-liberation
```

#### C) Install pyenv + Python

```bash
# Install pyenv
curl https://pyenv.run | bash

# Add pyenv to shells
lines_to_add='
export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
'
for file in ~/.bash_profile ~/.profile ~/.bashrc; do
  if ! grep -Fxq 'export PYENV_ROOT="$HOME/.pyenv"' "$file"; then
    echo "$lines_to_add" >> "$file"
  fi
done
source ~/.bashrc
source ~/.profile
[ -f ~/.bash_profile ] && source ~/.bash_profile

# Install Python and Jupyter kernel
pyenv install 3.13.7
pyenv global 3.13.7
pip install --upgrade pip
pip install jupyter ipykernel
```

#### D) Install R + IRkernel

```bash
# Add CRAN key/repo and install R
sudo apt update -qq
sudo apt install -y --no-install-recommends software-properties-common dirmngr wget gnupg

# Import CRAN GPG key
wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | \
  gpg --dearmor | sudo tee /etc/apt/trusted.gpg.d/cran_ubuntu_key.gpg > /dev/null

# Add CRAN repo for Ubuntu automatically, non-interactive
sudo add-apt-repository -y "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"

# Install R base and dev
sudo apt install -y --no-install-recommends r-base r-base-dev

# Make system-wide R libraries writable (team installs without renv)
sudo chmod -R 777 /usr/local/lib/R/site-library
sudo chmod -R 777 /usr/lib/R/site-library

# In R: install core packages + Jupyter kernel (parallelized, quiet)
sudo R --quiet -e 'Sys.setenv(MAKEFLAGS=paste0("-j", parallel::detectCores())); install.packages(c("languageserver","jsonlite","rlang","yaml","IRkernel","here"), repos="https://cloud.r-project.org"); IRkernel::installspec(user=FALSE)'

# Verify Jupyter sees the R kernel
jupyter kernelspec list

```

#### E) Shared data folder

```bash
sudo mkdir -p /home/data
sudo chmod -R 777 /home/data
sudo chown -R root:root /home/data
```

### 1.3 Per-User Setup

#### A) VS Code Remote Tunnel

> You may want to view serial console logs for the VM if the tunnel onboarding requires it.

```bash
sudo snap install code --classic
code tunnel  # Follow the link, sign in with GitHub, paste the one-time code
code tunnel service install
sudo loginctl enable-linger $USER
```

#### B) Git identity

```bash
git config --global user.name "Carlos Miguel Resurreccion"
git config --global user.email resurreccion.cmc@gmail.com
```

#### C) Clone repo + link shared data

```bash
git clone https://github.com/pids-drg-team/pids-drg-claims ~/pids-drg-claims
cd ~/pids-drg-claims
git submodule update --init --recursive

# Link shared data into the repo's expected path
sudo ln -s /home/data ~/pids-drg-claims/data-cleaning
```

---

## 2) Preparation: Downloading Claims & Creating Partials

You will run the **01-drg-partial.ipynb** notebook to pull raw claims from **Google Cloud Storage (GCS)**, split them into partial files, hash, and optionally create sample sets.

### 2.1 Security: Service Account Key

You need the file below **inside the repo**. Do **not** commit it to Git.

```
~/pids-drg-claims/data-cleaning/keys/pids-drg-data-25ad1e4c7298.json
```

> **Get this from a team member in private.** It grants access to PIDS GCP resources.

### 2.2 Verify parameters and packages

Open VS Code (connected via **Remote Tunnels**), then in the Explorer:

- Open `~/pids-drg-claims/data-cleaning/00a-parameters.r` → confirm values (years, paths, flags).
- Open `~/pids-drg-claims/data-cleaning/00b-packages.r` → confirm package list loads cleanly in the VM R kernel.

### 2.3 Run the partials notebook

Open `~/pids-drg-claims/data-cleaning/01-drg-partial.ipynb` and click **Run All**.

What it does (end‑to‑end):

1. **Load parameters**
2. **Load packages**
3. **Load full claims from GCS** (uses the service account key)
4. **Create partial files** (e.g., 15‑part / 30‑part RDS splits)
5. **Generate/Calculate/Validate file hashes**
6. **Create sample files** (optional, controlled by params)

> ✅ When this finishes, your `/home/data` (linked into the repo) should contain split artifacts and samples per your parameter settings.

---

## 3) Cleaning Proper (Per-Year Runs)

The per‑year cleaning notebook is `02-drg-cleaning-v3.ipynb`. It reads partials and produces cleaned, ready-for-grouping outputs and (optionally) uploads to **BigQuery**.

### 3.1 Verify starter scripts

Open and skim these to ensure they align with the VM environment:

- `00a-parameters.r` (again)
- `00b-packages.r`
- `00c-load-params-and-scripts.r`
- `00d-load-mapping.r`

### 3.2 Run the cleaner

Open `~/pids-drg-claims/data-cleaning/02-drg-cleaning-v3.ipynb` → **Run All**.

What it does:

1. Per‑year loop (driven by parameters)
2. Load parameters
3. Load packages
4. Load parameters + helper scripts
5. Load mapping tables
6. Define `process_chunk`
7. Run the **data cleaning loop**
8. Prepare datasets for **BigQuery**
9. Upload to **BigQuery** (if enabled in params)

### 3.3 All‑years runner (optional)

Open `~/pids-drg-claims/data-cleaning/experimental/clean-all-years.ipynb` → **Run All** to execute years in sequence unattended.

---

## 4) DRG Grouping

There are multiple grouping paths. Choose the one that fits your platform/resources.

### 4.1 Notebook (general): `03-drg-grouping-v2.ipynb`

- Works with the cleaned outputs from Step 3.
- Use this when you want an environment‑agnostic grouping (e.g., Python/R logic).

### 4.2 Thai Batch Grouper on Windows: `03a-drg-grouping-thai-v2.ps1`

Run on a **Windows 10/11** machine. Suggested beginner‑friendly prerequisites:

- **Windows 10/11, 64‑bit**, local admin account
- **PowerShell 5+** (default on Win10/11)
- **Execution Policy** to allow local scripts:
  ```powershell
  Set-ExecutionPolicy -Scope CurrentUser RemoteSigned
  ```
- **Visual FoxPro runtime** installed (required by the Thai DRG batch tools)
- **7-Zip** (optional; for archives)
- **Sufficient disk space** for intermediate TXT/CSV exports
- **Paths without spaces** recommended for the grouper working directory

Basic run:

1. Copy the needed **TXT** (exported for grouper), **mapping files**, and the **PS1** into a working folder (e.g., `C:\DRG_ThaiBatch`).
2. Right‑click **PowerShell** → **Run as Administrator**.
3. `cd C:\DRG_ThaiBatch`
4. Run:
   ```powershell
   .a-drg-grouping-thai-v2.ps1
   ```
5. Inspect the **output .txt/.csv** the grouper produces (keep originals; do not edit in Excel before merging).

> If you need the TXT exporter or merge helpers, see the companion R notebooks used earlier in this project for generating Thai batch input and merging results back.

### 4.3 Python BigQuery Grouping: `03b-drg-grouping-py-v2.ipynb` / `03c-drg-grouping-bq-v2.ipynb`

- `03b-…` handles Python‑assisted steps/pivots as needed.
- `03c-…` runs SQL‑side grouping/joins/aggregations in **BigQuery**.
- Use these when scaling or when Windows/Thai batch runtime is not available.

---

## 5) Checks

Open `~/pids-drg-claims/data-cleaning/04-drg-checks.ipynb` → **Run All**.

- Runs downstream QA/QC logic, sanity checks, and summary outputs for review.
- Save resulting artifacts in your `/home/data` linked paths or export to BQ per parameters.

---

## 6) Maintenance (VM)

Keep the VM patched periodically:

```bash
sudo apt update
sudo apt -y upgrade
sudo apt -y full-upgrade
sudo apt -y autoremove
```

---

## 7) Quick Troubleshooting

**A. R kernel not visible in Jupyter (VS Code)**

- Re-run the IRkernel install command (Step 1.2 D).
- `jupyter kernelspec list` should show an R entry.
- Reload VS Code window (`Ctrl+Shift+P` → “Developer: Reload Window”).

**B. GCS access denied in 01-drg-partial**

- Ensure the service account key JSON is at `data-cleaning/keys/…json`.
- Verify that the environment variable (if used) or path in the notebook points to that file.

**C. Out-of-memory during cleaning**

- Work per‑year; keep other processes idle.
- Consider scaling to a larger machine type temporarily (e.g., `n2d-highmem-32`).

**D. BigQuery upload errors**

- Verify dataset/table names in params.
- Confirm the GCP project and service account permissions (BigQuery Data Editor, Storage Admin as applicable).

**E. Thai batch grouper crashes**

- Reinstall Visual FoxPro runtime.
- Confirm file encodings (exported TXT must be unmodified).
- Avoid long/spacey paths; use `C:\DRG_ThaiBatch\…`.

---

## 8) Recap Checklist

1. **Create VM** in GCP and apply system-wide setup (Python, R, Jupyter).
2. **Enable Remote Tunnel**, set **Git identity**, **clone repo**, link `/home/data`.
3. Add **service account key** (private) under `data-cleaning/keys/…json`.
4. Run **01-drg-partial.ipynb** → GCS pull → partials → hashes → samples.
5. Run **02-drg-cleaning-v3.ipynb** → per‑year cleaning → (optional) upload to **BigQuery**.
6. Run grouping via **03-\*** notebooks or **03a PS1 (Thai batch on Windows)**.
7. Run **04-drg-checks.ipynb** for QA/QC.
8. Maintain the VM with periodic `apt` updates.
