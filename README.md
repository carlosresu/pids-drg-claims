# Important Notes:
The following should be in these locations after pulling the repo from github
raw claims files should be in data-cleaning/raw_claims/
Excel files should be in upload-libraries/Excel/

# VM Creation and Configuration

## VM Creation

Run this code in Google Cloud Platform Cloud Shell:

Currently, it is configured to have VS Code Server Code Tunnel accessible by the user profile of Carlos Resurreccion (resurreccion_cmc_gmail_com).

You can change this in the --metadata portion of the script below.

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

## VM Configuration

