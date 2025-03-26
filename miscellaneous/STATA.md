# VM Creation

## Creation Steps:

1. GCP > Vertex AI Workbench > Create New button (under the Instances tab) > Advanced options button (located in the pop-up window to the right)
2. Details (if not specified, leave default setting as is)
   1. Name: pids-drg-stata
   2. Region: us-central1 (Iowa)
   3. Zone: us-central1-a
   4. JupyterLab Version: JupyterLab 4.x
   5. VM specs: n2d-standard-8 (N2D CPU type, 8 vCPU, 32 GB memory)
      1. or N2 CPU type (i.e., n2-standard-8) if N2D is max quota'd already
      2. If you need more memory, select n2d-highmem-8 (64 GB)
      3. If you need EVEN more memory, select n2d-highmem-16 (128 GB) or higher
         1. NOTE: Current stata license only utilizes 8 cpu cores.
         2. NOTE: Please do this sparingly, as CPUs (that we can't even use) are expensive!
   6. Secure Boot: ✅ checked
   7. Idle Shutdown: 720 minutes (aka 12 hours)
   8. Boot Disk
      1. Type: Standard
      2. Size: 150 GB
   9. Data Disk
      1. Type: Standard
      2. Size: 250 GB
   10. Delete to Trash: ✅ checked
   11. Report custom metrics to Cloud Monitoring: ✅ checked
   12. Install Cloud Monitoring: ✅ checked
3. Create the VM! Then proceed to below steps.

## Configuration Steps

```
sudo apt update
sudo apt upgrade -y
sudo apt install -y xfce4 xfce4-goodies
```

```
wget https://dl.google.com/linux/direct/chrome-remote-desktop_current_amd64.deb
sudo dpkg -i chrome-remote-desktop_current_amd64.deb
sudo apt install -f -y
```

```
echo "exec xfce4-session" > ~/.chrome-remote-desktop-session
sudo apt remove -y light-locker
```

```
if ! getent group chrome-remote-desktop > /dev/null; then
  sudo groupadd chrome-remote-desktop
fi
sudo usermod -a -G chrome-remote-desktop $USER
sudo systemctl enable chrome-remote-desktop@$USER
sudo systemctl start chrome-remote-desktop@$USER
```

```
sudo mkdir -p /etc/skel/Desktop
sudo ln -sfn /home/jupyter /etc/skel/Desktop/jupyter
echo 'export PATH="/usr/local/stata18:$PATH"' | sudo tee -a /etc/skel/.bashrc
```

## Pairing Chrome Remote Desktop

### Get the below code from chrome remote desktop web interface, paste it in terminal via SSH https://remotedesktop.google.com/access/ (Set up via SSH -> Follow the steps)

```
DISPLAY= /opt/google/chrome-remote-desktop/start-host --code="YOUR*UNIQUE_CODE" --redirect-url="https://remotedesktop.google.com/*/oauthredirect" --name=$(hostname)
```

### Set a PIN: During execution, you’ll be prompted to enter and confirm a 6-digit PIN. This PIN will be used to authenticate when connecting remotely.

### When first connecting it'll ask for the admin password for the user jupyter for something. Just cancel/exit it.

## Stata Installation

### Install Stata (via ICTSD) to /usr/local/stata18

### Then add shortcut to all future users

```
sudo mkdir -p /etc/skel/Desktop
sudo tee /etc/skel/Desktop/stata.desktop > /dev/null <<EOF
[Desktop Entry]
Name=Stata MP
Comment=Launch Stata MP GUI
Exec=/usr/local/stata18/xstata-mp
Icon=utilities-terminal
Terminal=false
Type=Application
Categories=Education;
EOF
sudo chmod +x /etc/skel/Desktop/stata.desktop
```

# Stata Cloning

## Transfer from old VM

### creating the backup

```
sudo tar -cvpzf stata18_backup.tar.gz /usr/local/stata18 ~/.stata18
```

### gcloud steps (Login with an account that has access to gs://pids-drg-vm/stata)

```
gcloud init
gcloud storage cp ~/stata18_backup.tar.gz gs://pids-drg-vm/stata/
```

## On new VM

### gcloud steps (Login with an account that has access to gs://pids-drg-vm/stata)

```
gcloud init
gcloud storage cp gs://pids-drg-vm/stata/stata18_backup.tar.gz ~/
```

### Stata Restoration and Testing (should end with stata opening up; type exit to exit)

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
/usr/local/stata18/stata-mp
```

### Then add stata shortcut to all future users

```
sudo mkdir -p /etc/skel/Desktop
sudo tee /etc/skel/Desktop/stata.desktop > /dev/null <<EOF
[Desktop Entry]
Name=Stata MP
Comment=Launch Stata MP GUI
Exec=/usr/local/stata18/xstata-mp
Icon=utilities-terminal
Terminal=false
Type=Application
Categories=Education;
EOF
sudo chmod +x /etc/skel/Desktop/stata.desktop
```

# Transfer /home/ contents

## On Old VM

### creating the backup

```
sudo tar -czpvf /tmp/home_backup.tar.gz /home/jupyter
```

### gcloud steps (Login with an account that has access to gs://pids-drg-vm/home)

```
gcloud init
gcloud storage cp /tmp/home_backup.tar.gz gs://pids-drg-vm/home
```

## On New VM

### gcloud steps (Login with an account that has access to gs://pids-drg-vm/home)

```
gcloud init
gcloud storage cp gs://pids-drg-vm/home/home_backup.tar.gz /tmp/
```

### File restoration (ignore default folders, while restoring only /home/jupyter)

```
sudo tar -xzpvf /tmp/home_backup.tar.gz -C / home/jupyter \
  --exclude='home/jupyter/.*' \
  --exclude='home/jupyter/unix' \
  --exclude='home/jupyter/unix/*' \
  --exclude='home/jupyter/tutorials' \
  --exclude='home/jupyter/tutorials/*' \
  --exclude='*Trash*' \
  --exclude='*.ipynb_checkpoints*'
```

### Fixing file permissions

#### Do this so all future user-files in /home/jupyter have 777 (directories) or 666 (files)

```
sudo nano /home/jupyter/.jupyter/jupyter_notebook_config.py
```

#### Add the below

```
import os
os.umask(0)
```

#### Do this so existing user-files in /home/jupyter we restored have 777 (directories) or 666 (files), and so files in /home/jupyter are owned by the jupyter user

```
sudo find /home/jupyter -type d ! -path '*/.*' ! -name '.*' -exec sudo chmod 777 {} +
sudo find /home/jupyter -type f ! -path '*/.*' ! -name '.*' -exec sudo chmod 666 {} +
sudo chown -R jupyter:jupyter /home/jupyter
```

# Wrap up

On old and new VM's, run this to revoke your gmail account's login credentials so the VM reverts back to using the service account.

```
gcloud auth revoke your.email@gmail.com
```
