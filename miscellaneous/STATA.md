# VM Creation

# Creation Steps:

Guide to Creating a Vertex AI Workbench Instance with Specified Parameters

This guide outlines the steps to create a Vertex AI Workbench instance in Google Cloud Platform (GCP) using the provided configuration.

Step 1: Access the Vertex AI Workbench Console 1. Navigate to the Vertex AI Workbench page in the GCP Console. 2. Ensure you are in the correct GCP project where you have billing enabled.

Step 2: Begin Instance Creation

1. Click “New notebook”.
2. Choose “Customize” to configure a new notebook instance with your specific parameters.

Step 3: Configure Instance Details

1. Notebook Name:
   • Enter stata-vm-wbi.
   • Ensure the name starts with a letter and contains up to 47 lowercase letters, numbers, or hyphens, without ending in a hyphen.
2. Region and Zone:
   • Set Region to us-central1 (Iowa).
   • Set Zone to us-central1-a.
3. Enable Dataproc Serverless Interactive Sessions:
   • Check the box to enable Dataproc Spark kernels.
4. Labels and Tags (optional):
   • Add any labels or network tags if needed for identification or resource grouping.

Step 4: Configure Environment

1. Workbench Type:
   • Leave the default Instance selected.
2. Environment:
   • Leave “Use custom container” unchecked
   • Set the environment to “Use the latest version”.
3. Post-startup Script (optional):
   • Provide the path to a script in a Cloud Storage bucket if you want specific commands to run after the instance boots up.
4. Metadata:
   • Avoid using reserved metadata keys like data-disk-uri, framework, notebooks-api, etc.

Step 5: Configure Machine Type and Resources

1. Machine Type:
   • Choose e2-highmem-16:
   • vCPUs: 16 (8 cores).
   • Memory: 128 GB.
2. GPU:
   • Skip GPU configuration unless needed for additional workloads.
3. Shielded VM Options:
   • Enable all settings for enhanced security:
   • Secure Boot.
   • Virtual Trusted Platform Module (vTPM).
   • Integrity monitoring.
4. Idle Shutdown:
   • Enable Idle Shutdown and set inactivity time to 120 minutes (2 hours).

Step 6: Configure Disk Settings

1. Boot Disk:
   • Type: Standard Persistent Disk.
   • Size: 200 GB.
2. Data Disk:
   • Type: Standard Persistent Disk.
   • Size: 150 GB.
3. Encryption:
   • Use the Google-managed encryption key.

Step 7: Networking Configuration

1. Networking:
   • Ensure the default network is selected:
   • Network: default.
   • Subnetwork: default (10.128.0.0/20).
2. Assign External IP Address:
   • Enable to allow internet access.
3. Allow Proxy Access:
   • Ensure proxy access is enabled to access the instance via JupyterLab.
4. Private Google Access:
   • Leave this option turned off since the instance will use an external IP.

Step 8: Set IAM and Security

1. Service Account:
   • Use the default Compute Engine service account unless a specific account is required.
   • Ensure the account has sufficient API permissions.
2. Single User Access:
   • Restrict access to a single user by enabling Single user.
3. Security Options:
   • Allow:
   • Root access.
   • nbconvert for exporting notebooks.
   • File downloading.
   • Terminal access to run shell commands.

Step 9: Set System Health

1. Environment Auto-Upgrade
   • Leave this unchecked
2. Check Report System Health and Report DNS status for required Google Domains

Step 10: Review and Create

1. Double-check all configuration settings to ensure they match your requirements.
2. Click “Create” and wait for the instance to be provisioned.

Additional Notes

    •	Once the instance is created, you can SSH into it or access the JupyterLab interface for further customization.
    •	Ensure to stop or shut down the instance when not in use to avoid unnecessary charges.

This configuration creates a Vertex AI Workbench instance tailored for your specified parameters and optimized for flexibility, cost, and security.

# Configuration Steps

```
sudo apt update
sudo apt upgrade -y
sudo apt install -y xfce4 xfce4-goodies
wget https://dl.google.com/linux/direct/chrome-remote-desktop_current_amd64.deb
sudo dpkg -i chrome-remote-desktop_current_amd64.deb
sudo apt install -f -y
```

```
echo "exec xfce4-session" > ~/.chrome-remote-desktop-session
sudo apt remove light-locker
sudo usermod -a -G chrome-remote-desktop $USER
sudo systemctl enable chrome-remote-desktop@$USER
sudo systemctl start chrome-remote-desktop@$USER
```

Get the below code from chrome remote desktop web interface, paste it in terminal via SSH https://remotedesktop.google.com/access/ (Set up via SSH -> Follow the steps)
DISPLAY= /opt/google/chrome-remote-desktop/start-host --code="YOUR*UNIQUE_CODE" --redirect-url="https://remotedesktop.google.com/*/oauthredirect" --name=$(hostname)

Set a PIN:
During execution, you’ll be prompted to enter and confirm a 6-digit PIN.
This PIN will be used to authenticate when connecting remotely.

When first connecting it'll ask for the admin password for the user jupyter

```
sudo passwd jupyter
```

Enter a new password and remember it

Install stata18-mp to /usr/local/stata18

```
sudo nano ~/.bashrc
```

```
export PATH="/usr/local/stata18:$PATH"
```

```
source ~/.bashrc
```

```
cp /usr/local/stata18/stata.desktop ~/Desktop/
chmod +x ~/Desktop/stata.desktop
```

```
sudo nano ~/Desktop/stata.desktop
```

Locate the "Exec=" line
Make sure it points to xstata-mp

```
Exec=/usr/local/stata18/xstata-mp
```

Alternatively, just launch sxtata-mp via the terminal each time, after you've added it to path.
(By typing /usr/local/stata18/xstata-mp)

# Transfer /home/ contents

## On Old VM

```
sudo chmod -R 777 /home/
sudo tar -czpvf /tmp/home_backup.tar.gz /home/
```

```
gcloud init
```

Login with an account that has access to gs://pids-drg-data/vm/home

```
gcloud storage cp /tmp/home_backup.tar.gz gs://pids-drg-data/vm/home
```

## On New VM

```
gcloud init
sudo chmod -R 777 /home/
```

Login with an account that has access to gs://pids-drg-data/vm/home

```
gcloud storage cp gs://pids-drg-data/vm/home/home_backup.tar.gz /tmp/
sudo tar -xzpvf /tmp/home_backup.tar.gz -C / --exclude='*Trash*'
sudo chmod -R 777 /home/
```

# Stata Installation

## Transfer from old VM

```
sudo tar -cvpzf stata18_backup.tar.gz /usr/local/stata18 ~/.stata18
```

```
gcloud init
```

Login with an account that has access to gs://pids-drg-data/vm/stata

## On new VM

```
gcloud init
```

Login with an account that has access to gs://pids-drg-data/vm/stata

```
gcloud storage cp gs://pids-drg-data/vm/stata/stata18_backup.tar.gz ~/
```

```
sudo tar -xvpzf stata18_backup.tar.gz -C /
sudo chown -R root:root /usr/local/stata18
sudo chmod -R 777 /usr/local/stata18
sudo apt update
sudo apt install -y \
    libncurses5 libncurses5-dev libncursesw5 libtinfo5 \
    libpng-dev \
    libgtk2.0-0 libgtk2.0-dev \
    libgtk-3-0 libgtk-3-dev \
    libxtst6 libxext6 libxt6 libsm6 libice6 \
    libxmu6 libx11-6 libxrender1 \
    libglib2.0-0 libglib2.0-dev \
    libpango-1.0-0 libpango1.0-dev \
    libcairo2 libcairo2-dev \
    libxinerama1 libxi6 libxrandr2 \
    libcurl4 libcurl4-openssl-dev
```

## Wrap up

On old and new VM's, run this to revoke your gmail account's login credentials so the VM reverts back to using the service account.

```
gcloud auth revoke your.email@gmail.com
```
