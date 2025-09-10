# VM Creation and Configuration

This VM is configured **only** for running Python and R code, via **gcloud SSH** or **Jupyter notebooks through VS Code Remote Tunnels**.

- **Included:** Jupyter, pyenv, R + IRkernel, shared data folder `/home/data`
- **Excluded:** Chrome Remote Desktop, .NET, second-disk setup, patch-job/maintenance automations.

---

## 1) VM Creation

Run in **GCP Cloud Shell**:

1. **Create instance**

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

> **Notes**
>
> * Leaves **deletion protection** **ON**. Disable manually if you need to delete later.
> * Leaves **lb-health-check** tag (harmless if unused).

Enable VM Manager features (once per project):

```bash
gcloud compute os-config project-feature-settings update \
  --project pids-drg-data \
  --patch-and-config-feature-set=full

gcloud compute os-config project-feature-settings describe \
  --project pids-drg-data
```

---

## 2) VM Configuration (System-Wide)

SSH into the VM, then run:

### 2.1 Basic system prep

```bash
# Parallel builds for source-compiled packages
echo 'export MAKEFLAGS="-j$(nproc)"' | sudo tee -a /etc/environment
source /etc/environment

# Update base OS
sudo apt update
sudo apt upgrade -y
```

### 2.2 Data-science packages for Python & R

```bash
# -------------------------------
# Data science essentials for Python & R
# (Numerics, build toolchains, compression, plotting — no GIS)
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

# Lightweight plotting stack for R/matplotlib (no GIS)
sudo apt install -y \
  libcairo2-dev \
  libpng-dev \
  libjpeg-dev \
  libtiff5-dev \
  libfreetype6-dev \
  libfontconfig1-dev
```

### 2.3 Install pyenv + Python

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

### 2.4 Install R + IRkernel

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

### 2.5 Shared data folder

```bash
sudo mkdir -p /home/data
sudo chmod -R 777 /home/data
sudo chown -R root:root /home/data
```

---

## 3) Per-User Setup

### 3.1 VS Code Remote Tunnel

```bash
sudo snap install code --classic
code tunnel # Follow link, auth with GitHub, enter code in terminal
code tunnel service install
sudo loginctl enable-linger $USER
```

### 3.2 Git identity

```bash
git config --global user.name "Carlos Miguel Resurreccion"
git config --global user.email resurreccion.cmc@gmail.com
```

### 3.3 Repo + data link

Use your GitHub username, and generate a personal access token (classic) with project (all) and repo (all) permissions.

```bash
git clone https://github.com/pids-drg-team/pids-drg-claims ~/pids-drg-claims
cd ~/pids-drg-claims
git submodule update --init --recursive

# Link shared data into the repo's expected path
sudo ln -s /home/data ~/pids-drg-claims/data-cleaning
```

---

## 4) Running the Pipeline (Notebook)

1. Connect via **VS Code → Remote Tunnels** → `drg-data-pipeline` (or your tunnel name).
2. Open: `~/pids-drg-claims/data-cleaning/drg-cleaning.ipynb`.
3. Confirm parameters and that `/home/data` is linked.
4. **Run All**. Follow on-notebook prompts for Thai Batch Grouper as needed.

---

## 5) Quick sanity checks (optional)

```bash
# Python: confirm BLAS/LAPACK and NumPy health
python - <<'PY'
import numpy as np, sys
print("Python:", sys.version)
a = np.random.rand(2000,2000)
b = a @ a.T
print("NumPy OK, shape:", b.shape)
PY

# R: basic session and BLAS
R -q -e 'sessionInfo(); capabilities(); sessionInfo()$BLAS'
```
