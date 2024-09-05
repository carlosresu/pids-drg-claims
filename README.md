# VM Creation and Configuration

## VM Creation

Run this code in Google Cloud Platform Cloud Shell:

1. Currently, it is configured to have VS Code Server Code Tunnel accessible by the user profile of Carlos Resurreccion (resurreccion_cmc_gmail_com). (See --metadata portion of the script below.)
2. Service account should be the service account of the GCP Project. (See --service-account portion of the script below.)

```
gcloud compute instances create drg-data-pipeline \
  --project=drg-pipeline \
  --zone=us-central1-a \
  --machine-type=e2-highmem-8 \
  --network-interface=network-tier=PREMIUM,stack-type=IPV4_ONLY,subnet=default \
  --metadata=enable-osconfig=TRUE,startup-script=\#\!/bin/bash$'\n'sudo\ apt-get\ update\ -y$'\n'sudo\ apt-get\ upgrade\ -y$'\n'USER=\"resurreccion_cmc_gmail_com\"$'\n'sudo\ -u\ \$USER\ bash\ -c\ \'code\ tunnel\',enable-oslogin=TRUE \
  --maintenance-policy=MIGRATE \
  --provisioning-model=STANDARD \
  --service-account=271591364028-compute@developer.gserviceaccount.com \
  --scopes=https://www.googleapis.com/auth/cloud-platform \
  --tags=http-server,https-server,lb-health-check \
  --create-disk=auto-delete=yes,boot=yes,device-name=drg-data-pipeline,image=projects/ubuntu-os-cloud/global/images/ubuntu-2404-noble-amd64-v20240809,mode=rw,size=200,type=pd-ssd \
  --shielded-secure-boot \
  --shielded-vtpm \
  --shielded-integrity-monitoring \
  --labels=goog-ec-src=vm_add-gcloud \
  --reservation-affinity=any
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
# Update the package list:
sudo apt update

# Install Jupyter:
sudo apt install jupyter jupyter-core jupyter-client build-essential libcurl4-openssl-dev libssl-dev libxml2-dev libsodium-dev python3-full python3-pip

# upgrade packages
sudo apt upgrade
```

Install R

```
# Install R
## Update package list
sudo apt update

## Install R 4.4.1 dependencies
sudo apt install -y software-properties-common dirmngr

## Add CRAN GPG Key
wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc

## Add CRAN repository
sudo add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu noble-cran40/'

## Update package list again
sudo apt update

## Install R 4.4.1
sudo apt install -y r-base

## Check R Version
R --version
```

Make system-wide libraries writable by R, otherwise we'd need to rely on renv which we've been unable to get working.

Here we create a data folder in the home directory where multiple users can store data, accessible to all of their user profile git cloned repositories. 

For example, if I login as resurreccion_cmc_gmail_com, my user profile folder is /home/resurreccion_cmc_gmail_com and in that is my drg-pipeline git cloned repository. Later we will symbolically link the entire 'data' folder to each of our git cloned repository folders, as the code expects the 'data' folder and its contents to be in the data-cleaning folder of the repository, i.e. ~/drg-pipeline/data-cleaning/data.

```
sudo chmod -R 777 /usr/local/lib/R/site-library
sudo chmod -R 777 /usr/lib/R/site-library
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

Run the below code in Terminal, *inside of R.*
```
install.packages("languageserver")
install.packages("jsonlite")
install.packages("rlang")
install.packages("yaml")
install.packages("IRkernel")
install.packages("here")
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

sudo apt-get install apt-transport-https ca-certificates gnupg curl

curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo gpg --dearmor -o /usr/share/keyrings/cloud.google.gpg

echo "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] https://packages.cloud.google.com/apt cloud-sdk main" | sudo tee -a /etc/apt/sources.list.d/google-cloud-sdk.list

sudo apt-get update && sudo apt-get install google-cloud-cli

gcloud init

# Login with the service account
```

Lastly, edit the VM instance in GCP and add the following in the text box of the startup script automation section:

```
#!/bin/bash
sudo apt-get update -y
sudo apt-get upgrade -y
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
```

Symbolically Link /home/data to your username's drg-pipeline/data-cleaning folder

```
sudo ln -s /home/data /home/resurreccion_cmc_gmail_com/drg-pipeline/data-cleaning
```

Clone grouper into your home folder first

```
git clone https://github.com/pids-drg/grouper
```

Link /home/resurreccion_cmc_gmail_com/grouper to /home/resurreccion_cmc_gmail_com/drg-pipeline

```
sudo ln -s /home/resurreccion_cmc_gmail_com/grouper /home/resurreccion_cmc_gmail_com/drg-pipeline/data-cleaning
```

Link /home/resurreccion_cmc_gmail_com/grouper contents into /home/resurreccion_cmc_gmail_com/drg-pipeline/data-cleaning as the script (reticulate) expects it to be there.

```
sudo ln -s /home/resurreccion_cmc_gmail_com/grouper/libraries /home/resurreccion_cmc_gmail_com/drg-pipeline/data-cleaning
sudo ln -s /home/resurreccion_cmc_gmail_com/grouper/misc /home/resurreccion_cmc_gmail_com/drg-pipeline/data-cleaning
sudo ln -s /home/resurreccion_cmc_gmail_com/grouper/scripts /home/resurreccion_cmc_gmail_com/drg-pipeline/data-cleaning
sudo ln -s /home/resurreccion_cmc_gmail_com/grouper/tests /home/resurreccion_cmc_gmail_com/drg-pipeline/data-cleaning
```

To use Google Cloud Code, press sign in inside the VS Code extension, it'll open a webbrowser and try to open a localhost link. It won't work as this will open on your local machine instead of the VM. Just copy the link, then open the VM terminal via SSH via GCP, then type "curl \<link\>"

# How To: Run Data Cleaning Code End-to-End

By end-to-end, we mean from GCS pull of raw claims files, to BQ push of claims after cleaning and then grouping.

Assuming you've already authorized the VS Code Server Code Tunnel in the VM, simply open your local VS Code install (with the Remote Development Extension from Microsoft), then 
1. Click the `\>\<` button on the bottom left corner of VS Code, and 
2. Press `Connect to Tunnel`, then 
3. Press `GitHub`, then 
4. Press `drg-data-pipelineus-`

Once inside, 
1. Select the `drg-pipeline` folder in your user directory that we created by cloning the `drg-pipeline` repo earlier
2. Open `data-cleaning/drg-cleaning.ipynb`

Finally,
1. Go over the parameters under `Primary` and `Secondary Parameters`, as well as `File Paths`, and 
2. Make sure everything is in order.

**Important 1: Ensure you've symbolically linked `/home/data` to `/home/\<username\>/drg-pipeline/data-cleaning`**
**Important 2: Ensure you've symbolically linked `/home/\<username\>/grouper` (i.e. `./libraries`, `./misc`, `./scripts`, and `./tests`) to `/home/\<username\>/drg-pipeline/data-cleaning`**

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
6.  It should now proceed with the process, first by analyzing and checking for differences between the drg code generated via Python Grouper vs via Thai Batch Grouper.
    1.  It will write a csv containing said differences (or an empty csv if there are none), 
    2.  It will write to `~/drg-pipeline/data/checkpoints/checkpoint_9_grouper_differences` as `checkpoint_9_grouper_differences_*.csv`
7.  It will then push to BQ as `drg-pipeline.phic.claims_20XX1231`