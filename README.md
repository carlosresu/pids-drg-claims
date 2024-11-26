# VM Creation and Configuration

## VM Creation

Run this code in Google Cloud Platform Cloud Shell:

1. Currently, it is configured to have VS Code Server Code Tunnel accessible by the user profile of Carlos Resurreccion (resurreccion_cmc_gmail_com). (See --metadata portion of the script below.)
2. Service account should be the service account of the GCP Project. (See --service-account portion of the script below.)

First time
```
gcloud compute instances create drg-data-pipeline \
    --project=drg-pipeline \
    --zone=us-central1-a \
    --machine-type=e2-highmem-16 \
    --network-interface=network-tier=PREMIUM,stack-type=IPV4_ONLY,subnet=default \
    --can-ip-forward \
    --maintenance-policy=MIGRATE \
    --provisioning-model=STANDARD \
    --service-account=271591364028-compute@developer.gserviceaccount.com \
    --scopes=https://www.googleapis.com/auth/cloud-platform \
    --tags=http-server,https-server,lb-health-check \
    --create-disk=auto-delete=yes,boot=yes,device-name=drg-data-pipeline-boot-disk,image=projects/ubuntu-os-cloud/global/images/ubuntu-2404-noble-amd64-v20241115,mode=rw,size=20,type=pd-ssd \
    --create-disk=device-name=drg-data-pipeline-data-disk-v2,mode=rw,name=drg-data-pipeline-data-disk-v2,size=150,type=pd-ssd \
    --shielded-secure-boot \
    --shielded-vtpm \
    --shielded-integrity-monitoring \
    --labels=goog-ec-src=vm_add-gcloud \
    --reservation-affinity=any \
    --deletion-protection
```

Subsequent creations:
```
gcloud compute instances create drg-data-pipeline-v3 \
    --project=drg-pipeline \
    --zone=us-central1-a \
    --machine-type=e2-highmem-8 \
    --network-interface=network-tier=PREMIUM,stack-type=IPV4_ONLY,subnet=default \
    --metadata=enable-oslogin=true \
    --can-ip-forward \
    --maintenance-policy=MIGRATE \
    --provisioning-model=STANDARD \
    --service-account=271591364028-compute@developer.gserviceaccount.com \
    --scopes=https://www.googleapis.com/auth/cloud-platform \
    --tags=http-server,https-server,lb-health-check \
    --create-disk=auto-delete=yes,boot=yes,device-name=drg-data-pipeline-boot-disk,image=projects/ubuntu-os-cloud/global/images/ubuntu-2404-noble-amd64-v20241115,mode=rw,size=20,type=pd-ssd \
    --disk=boot=no,device-name=drg-data-pipeline-data-disk,mode=rw,name=drg-data-pipeline-data-disk \
    --shielded-secure-boot \
    --shielded-vtpm \
    --shielded-integrity-monitoring \
    --labels=goog-ec-src=vm_add-gcloud \
    --reservation-affinity=any \
    --deletion-protection
```

Run the below code in GCP Cloud Shell. This enables the Patch service to work with the VM.

```
# Enable full VM Manager features
gcloud compute os-config project-feature-settings update --project drg-pipeline --patch-and-config-feature-set=full

# Verify it's working
gcloud compute os-config project-feature-settings describe --project drg-pipeline
```

## VM Configuration (System-Wide)

Run all the below code in Terminal (after SSH-ing into the VM via GCP) unless otherwise specified.

Update the package list, install jupyter, python, and build tools, then upgrade packages.

```
echo 'export MAKEFLAGS="-j$(nproc)"' | sudo tee -a /etc/environment
source /etc/environment

# Update the package list:
sudo apt update

# Install Jupyter:
sudo apt install -y jupyter jupyter-core jupyter-client libcurl4-openssl-dev libssl-dev libxml2-dev libsodium-dev pipx  libfontconfig1-dev libharfbuzz-dev libfribidi-dev libgeos-dev libudunits2-dev libgit2-dev python3-venv python3-dev libgfortran5 liblapack-dev libblas-dev libcairo2-dev libz-dev liblz4-dev libzstd-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev libgdal-dev libproj-dev libgmp3-dev libgmp-dev ncdu python3-full python3-pip npm pandoc

sudo apt install -y make build-essential libssl-dev zlib1g-dev libbz2-dev libreadline-dev libsqlite3-dev wget curl llvm libncursesw5-dev xz-utils tk-dev libxml2-dev libxmlsec1-dev libffi-dev liblzma-dev


# Install glances
sudo snap install glances

# Build tools
sudo apt-get update
sudo apt-get install -y build-essential libprotobuf-dev

# upgrade packages
sudo apt upgrade

# Install Microsoft .NET 8.0
sudo apt-get update && sudo apt-get install -y dotnet-sdk-8.0
```

```
# install pyenv
curl https://pyenv.run | bash
```

# Insert the following in ~/.bash_profile, ~/.profile ~/.bashrc
export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

```
# Define the lines to add
lines_to_add='
export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
'

# Add the lines to each file if not already present
for file in ~/.bash_profile ~/.profile ~/.bashrc; do
    if ! grep -Fxq 'export PYENV_ROOT="$HOME/.pyenv"' "$file"; then
        echo "$lines_to_add" >> "$file"
        echo "Added pyenv configuration to $file"
    else
        echo "pyenv configuration already exists in $file"
    fi
done

# Source the updated files to apply changes immediately
source ~/.bashrc
source ~/.profile
[ -f ~/.bash_profile ] && source ~/.bash_profile

echo "pyenv environment setup complete!"
```

```
# Install python 3.12.7
pyenv install 3.12.7
pyenv global 3.12.7
```

```
pip install jupyter jupyter-core jupyter-client ipykernel
```

```
# create a venv and install venv-reliant packages
# python3 -m venv ~/venv

# Activate the virtual environment
# source ~/venv/bin/activate
```

New Install R method
```
# update indices
sudo apt update -qq

# install two helper packages we need
sudo apt install -y --no-install-recommends software-properties-common dirmngr

# add the signing key (by Michael Rutter) for these repos
# To verify key, run gpg --show-keys /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc 
# Fingerprint: E298A3A825C0D65DFD57CBB651716619E084DAB9

wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc

# add the R 4.0 repo from CRAN -- adjust 'focal' to 'groovy' or 'bionic' as needed
sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"

## Install R 4.4.2
sudo apt install -y --no-install-recommends r-base r-base-dev

## Check R Version
R --version
```

Make system-wide libraries writable by R, otherwise we'd need to rely on renv which we've been unable to get working.

Here we create a data folder in the home directory where multiple users can store data, accessible to all of their user profile git cloned repositories.

For example, if I login as resurreccion_cmc_gmail_com, my user profile folder is /home/resurreccion_cmc_gmail_com and in that is my drg-pipeline git cloned repository. Later we will symbolically link the entire 'data' folder to each of our git cloned repository folders, as the code expects the 'data' folder and its contents to be in the data-cleaning folder of the repository, i.e. ~/drg-pipeline/data-cleaning/data.

```
sudo chmod -R 777 /usr/local/lib/R/site-library
sudo chmod -R 777 /usr/lib/R/site-library
```

```
lsblk
sudo mkfs.ext4 -F /dev/sdb # WARNING: ONLY FORMAT THE DISK IF NOT ALREADY FORMATTED
```

```
sudo mkdir -p /mnt/data-disk
sudo mount /dev/sdb /mnt/data-disk
sudo blkid /dev/sdb
# note UUID of data-disk
```

```
sudo nano /etc/fstab
```

```
UUID=bb716fcb-7f55-418c-8382-455bd288d54a /mnt/data-disk ext4 defaults 0 2
```


```
sudo mkdir -p /mnt/data-disk/data
sudo chmod -R 777 /mnt/data-disk/data
sudo chown -R root:root /mnt/data-disk/data
sudo chmod -R 777 /mnt/data-disk/data
```

Old Code
```
sudo mkdir -p /home/data
sudo chmod -R 777 /home/data
sudo chown -R root:root /home/data
sudo chmod -R 777 /home/data
```

Start R to install necessary packages that we need to work with R in VS Code, these cannot be installed later on as we will not be able to access R in our jupyter notebooks otherwise.

```
# To start R
sudo R
```

Install necessaary packages, then install the IR kernel system-wide. Note that this takes a while as these packages are compiled from source.

Run the below code in Terminal, _inside of R._

```
Sys.setenv(MAKEFLAGS = paste0("-j", parallel::detectCores()))
install.packages("languageserver", dependencies = TRUE)
install.packages("jsonlite", dependencies = TRUE)
install.packages("rlang", dependencies = TRUE)
install.packages("yaml", dependencies = TRUE)
install.packages("IRkernel", dependencies = TRUE)
install.packages("here", dependencies = TRUE)
# install.packages("rmarkdown") # may not be needed
# install.packages("reticulate") # may not be needed
IRkernel::installspec(user = FALSE)
```

Quit R to resume working in Terminal

```
quit()
```

Verify R is usable as a jupyter kernel

```
# Verify R jupyter kernel is usable
jupyter kernelspec list
```

Install gcloud CLI on the VM, login with the service account we spoke about earlier.

```
sudo apt-get update

sudo apt-get install -y apt-transport-https ca-certificates gnupg curl

curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo gpg --dearmor -o /usr/share/keyrings/cloud.google.gpg

echo "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] https://packages.cloud.google.com/apt cloud-sdk main" | sudo tee -a /etc/apt/sources.list.d/google-cloud-sdk.list

sudo apt-get update && sudo apt-get install -y google-cloud-cli

gcloud init

# Login with the service account

gcloud auth application-default login # MAY NOT BE NEEDED ANYMORE

# Login with the appropriate account # MAY NOT BE NEEDED ANYMORE

# Upload drg-pipeline-e80a2b3a9229.json (in Carlos Resurreccion's PIDS OneDrive) or an equivalent Service Account Json Key to VM at ~/.config/gcloud/
# If using another key, rename it to drg-pipeline-e80a2b3a9229.json all the same.
```

Lastly, edit the VM instance in GCP and add the following in the text box of the startup script automation section:

```
#!/bin/bash
USER="resurreccion_cmc_gmail_com"
sudo -u $USER bash -c 'code tunnel'
```

This runs code tunnel on VM startup.

TODO: make it work with multiple users.

# VM Configuration (Per User)

Run the below code in Terminal after SSH-ing into the VM via GCP. A link will appear in the Terminal output along with an authentication code in the format XXXX-XXXX, open it, login with your GitHub account that you'll use to code tunnel into the server with VS Code, then type in the authentication code.

```
sudo snap install code --classic
code tunnel
```

Configure git on the VM, change these to your user details if logging in from a user other than that which we configured earlier.

```
# Configure git on the VM
git config --global user.name "Carlos Miguel Resurreccion"
git config --global user.email resurreccion.cmc@gmail.com
```

Clone drg-pipeline into your home folder first

```
git clone https://github.com/pids-drg/drg-pipeline
cd ~/drg-pipeline
git submodule update --init --recursive
```

Symbolically Link /mnt/data-disk/data to your username's drg-pipeline/data-cleaning folder

```
sudo ln -s /mnt/data-disk/data /home/resurreccion_cmc_gmail_com/drg-pipeline/data-cleaning
```

Link ~/drg-pipeline/data-cleaning/grouper/libraries contents into ~/drg-pipeline/data-cleaning as the script (reticulate) expects it to be there.

```
sudo ln -s ~/drg-pipeline/data-cleaning/grouper/libraries ~/drg-pipeline/data-cleaning
sudo ln -s ~/drg-pipeline/data-cleaning/grouper/scripts ~/drg-pipeline/data-cleaning
sudo ln -s ~/drg-pipeline/data-cleaning/grouper/misc ~/drg-pipeline/data-cleaning
sudo ln -s ~/drg-pipeline/data-cleaning/grouper/tests ~/drg-pipeline/data-cleaning
```

Configure ipykernel with venv
Create a .venv using VS Code Python: Select Interpreter > Create a Virtual Environment > .venv > select requirements.txt in data-cleaning (not the grouper).

To use Google Cloud Code, press sign in inside the VS Code extension, it'll open a webbrowser and try to open a localhost link. It won't work as this will open on your local machine instead of the VM. Just copy the link, then open the VM terminal via SSH via GCP, then type `curl <link>`

# How To: Run Data Cleaning Code End-to-End

By end-to-end, we mean from GCS pull of raw claims files, to BQ push of claims after cleaning and grouping.

Assuming you've already authorized the VS Code Server Code Tunnel in the VM, simply open your local VS Code install (with the Remote Development Extension from Microsoft), then

1. Click the `><` button on the bottom left corner of VS Code, and
2. Press `Connect to Tunnel`, then
3. Press `GitHub`, then
4. Press `drg-data-pipeline-v3`

Once inside,

1. Select the `drg-pipeline` folder in your user directory that we created by cloning the `drg-pipeline` repo earlier
2. Open `data-cleaning/drg-cleaning.ipynb`

Finally,

1. Go over the parameters under `Primary` and `Secondary Parameters`, as well as `File Paths`, and
2. Make sure everything is in order.

**Important 1: Ensure you've symbolically linked `/mnt/data-disk/data` to `/home/<username>/drg-pipeline/data-cleaning`**

**Important 2: Ensure you've symbolically linked `~/drg-pipeline/data-cleaning/grouper/libraries` to `~/drg-pipeline/data-cleaning`**

Steps to run the data-cleaning code end-to-end:

1. Run the notebook via VS Code's Run All button
2. Wait for the code to clean the data. This should take about an hour.
3. Review the summary outputs to see if there are any anomalies that need addressing in the code.
   1. If there are none, you don't need to do anything to proceed.
   2. If there are anomalies, **stop the code now.**
4. Wait for the code to group the claims via the Python Grouper. It should take quite a few hours.
5. After it's done, it should then automatically prompt you asking if you've run the Thai Batch Grouper already.
   1. If you have,
      1. Type `y`.
      2. Press `enter`.
   2. If not:
      1. **Don't type anything or press enter just yet. Leave it pending.** **DO NOT CLOSE VS CODE OR DISCONNECT FROM THE CODE TUNNEL INSTANCE**
      2. Go to GCP GCS `phic-claims-checkpoints/pre-tdrg` (<https://console.cloud.google.com/storage/browser/phic-claims-checkpoints/pre-tdrg?project=drg-pipeline>)
      3. Find the file it just uploaded.
      4. Download the file to your local machine. **DO NOT RENAME THE FILE AFTER DOWNLOADING.**
      5. Run the Thai Batch Grouper `(TGRP50V02.exe)` on the file you just downloaded. It should take an hour or two.
      6. Go to GCP GCS `phic-claims-checkpoints/post-tdrg` (<https://console.cloud.google.com/storage/browser/phic-claims-checkpoints/post-tdrg?project=drg-pipeline>)
      7. Upload the file outputted by the Thai Batch Grouper. **DO NOT RENAME THE FILE BEFORE UPLOADING.**
      8. Return to your VS Code Code Tunnel Instance.
      9. Type `y`.
      10. Press `enter`.
6. It should now proceed with the process, first by analyzing and checking for differences between the drg code generated via Python Grouper vs via Thai Batch Grouper.
   1. It will write a csv containing said differences (or an empty csv if there are none),
   2. It will write to `~/drg-pipeline/data/checkpoints/checkpoint_9_grouper_differences` as `checkpoint_9_grouper_differences_*.csv`
7. It will then push to BQ as `drg-pipeline.phic_claims.claims_20XX1231`

# Maintenace

1. Enable scheduled shutdown
   1. (<https://console.cloud.google.com/compute/instances/instanceSchedules?project=drg-pipeline&tab=instanceSchedules>)
   2. Create a scheduler job, set it to Iowa, Philippine time, start empty, and stop at 8:30 PM. Name it stop-vm-eod.
   3. Add the VM instance to it, you'll need the following permissions:
      `Compute Engine System service account service-271591364028@compute-system.iam.gserviceaccount.com needs to have [compute.instances.stop] permissions applied in order to perform this operation.`
2. Enable patch job for updates
   `echo $'{\n  \"name\": \"projects/271591364028/patchDeployments/update-vm\",\n  \"instanceFilter\": {\n    \"instances\": [\"zones/us-central1-a/instances/drg-data-pipeline\"]\n  },\n  \"patchConfig\": {\n    \"rebootConfig\": \"DEFAULT\",\n    \"apt\": {\n      \"type\": \"DIST\"\n    },\n    \"yum\": {\n    },\n    \"zypper\": {\n    },\n    \"windowsUpdate\": {\n    }\n  },\n  \"duration\": \"3600s\",\n  \"recurringSchedule\": {\n    \"timeZone\": {\n      \"id\": \"Asia/Manila\"\n    },\n    \"timeOfDay\": {\n      \"hours\": 19,\n      \"minutes\": 30\n    },\n    \"frequency\": \"DAILY\"\n  },\n  \"rollout\": {\n    \"mode\": \"CONCURRENT_ZONES\",\n    \"disruptionBudget\": {\n      \"fixed\": 1\n    }\n  }\n}' > patch_deployment_96a2901c-46a5-4ef6-b9a1-d6e4bf6f96c3.json && gcloud compute os-config patch-deployments update update-vm --file=patch_deployment_96a2901c-46a5-4ef6-b9a1-d6e4bf6f96c3.json`
3. Add the following update-vm-post-patch-script.sh to gs://phic-other/update-vm-post-patch-script.sh

   ```
   #!/bin/bash

   # Ensure the script exits if any command fails
   set -e

   # Define the path to the r-reticulate virtual environment
   VENV_PATH="/home/resurreccion_cmc_gmail_com/.virtualenvs/r-reticulate"

   # Function to upgrade a virtual environment
   upgrade_venv() {
      VENV_BIN="$1/bin/python"
      if [[ -x "$VENV_BIN" ]]; then
         echo "Upgrading all packages in virtual environment: $1"
         "$VENV_BIN" -m pip install --upgrade pip setuptools
         "$VENV_BIN" -m pip freeze | cut -d '=' -f 1 | xargs -n1 "$VENV_BIN" -m pip install --upgrade || true
      fi
   }

   # Activate and upgrade the r-reticulate virtual environment
   echo "Activating and upgrading r-reticulate..."
   source "$VENV_PATH/bin/activate"
   pip install --upgrade pip setuptools
   pip install --upgrade numpy pandas streamlit python_dateutil tabulate swifter rpy2 pyreadr papermill nbformat IProgress jupyter ipywidgets
   deactivate

   # Upgrade all Python packages in every detected virtual environment
   echo "Searching for virtual environments..."
   find /home -type d -name 'bin' -path '*/.virtualenvs/*/bin' 2>/dev/null | while read -r bin_path; do
      upgrade_venv "$(dirname "$bin_path")"
   done

   # Function to upgrade a package using apt or pipx
   upgrade_package() {
      PACKAGE=$1
      echo "Trying to upgrade $PACKAGE via apt..."

      # Attempt to upgrade via apt
      if sudo apt install -y "python3-$PACKAGE" >/dev/null 2>&1; then
         echo "$PACKAGE upgraded via apt."
      else
         echo "Failed to upgrade $PACKAGE via apt. Trying pipx..."

         # Attempt to install/upgrade via pipx
         if pipx list | grep -q "$PACKAGE"; then
               echo "$PACKAGE already managed by pipx. Upgrading..."
               pipx upgrade "$PACKAGE" || echo "Failed to upgrade $PACKAGE with pipx."
         else
               echo "$PACKAGE not found in pipx. Installing..."
               pipx install "$PACKAGE" || echo "Failed to install $PACKAGE with pipx."
         fi
      fi
   }

   # Upgrade all installed packages for valid system Python versions
   echo "Searching for valid Python versions..."
   for python_bin in /usr/bin/python* /usr/local/bin/python*; do
      if [[ "$python_bin" =~ python[0-9.]+$ ]] && [[ -x "$python_bin" ]]; then
         echo "Upgrading all packages for $python_bin..."
         "$python_bin" -m pip install --upgrade pip setuptools --break-system-packages || true
         installed_packages=$("$python_bin" -m pip freeze | cut -d '=' -f 1)

         # Use apt or pipx for each package if available, otherwise use pip
         for pkg in $installed_packages; do
               if ! upgrade_package "$pkg"; then
                  echo "Upgrading $pkg via pip for $python_bin..."
                  "$python_bin" -m pip install --upgrade "$pkg" --break-system-packages || true
               fi
         done
      fi
   done

   echo "All Python packages across the system and virtual environments have been upgraded."
   ```
