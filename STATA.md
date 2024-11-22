# VM Creation

1. Make the VM via Vertex AI Instances
   1. (See Celina's steps)

```
sudo apt update
sudo apt install -y xfce4 xfce4-goodies
curl https://dl.google.com/linux/direct/chrome-remote-desktop_current_amd64.deb -o crdp.deb
sudo dpkg -i crdp.deb
sudo apt-get install -f
```

```
sudo nano ~/.bashrc
```

```
export PATH="/usr/local/stata18:$PATH"
```

```
source ~/.bashrc
```

Setting Up a Vertex AI Workbench VM with Stata 18 MP, XFCE, and Chrome Remote Desktop

Table of Contents

	1.	Prerequisites
	2.	Create a Vertex AI Workbench Instance
	3.	Install XFCE Desktop Environment
	4.	Install Chrome Remote Desktop
	5.	Install Stata 18 MP
	6.	Configure Chrome Remote Desktop
	7.	Access Your VM via Chrome Remote Desktop
	8.	Launch Stata 18 MP via Remote Desktop
	9.	References

Prerequisites

	•	Google Cloud Platform (GCP) Account: Ensure you have a GCP account with billing enabled.
	•	Stata 18 MP Installer and License: Obtain the Stata 18 MP installer and a valid license.
	•	Google Chrome Browser: Install Google Chrome on your local machine.
	•	Chrome Remote Desktop Extension: Install from the Chrome Web Store.

Create a Vertex AI Workbench Instance

	1.	Navigate to Vertex AI Workbench:
	•	Go to the Vertex AI Workbench page in the GCP Console.
	2.	Create a New Notebook Instance:
	•	Click on “New notebook”.
	•	Choose “Customize” to configure your VM settings.
	3.	Configure the VM:
	•	Notebook name: Enter a unique name (e.g., stata-vm).
	•	Region and Zone: Select your preferred location.
	•	Machine type: Choose a machine type suitable for Stata MP (e.g., n1-highmem-8 for 8 vCPUs).
	•	Boot disk:
	•	Type: Select Standard persistent disk or SSD.
	•	Size: Allocate sufficient space (e.g., 100 GB).
	4.	Networking:
	•	Under “Networking”, ensure default settings unless custom configurations are required.
	5.	Firewall Settings:
	•	Check “Enable” for “Allow HTTP traffic” and “Allow HTTPS traffic” if needed.
	6.	Create the Instance:
	•	Click “Create” and wait for the instance to be provisioned.

Install XFCE Desktop Environment

	1.	SSH into the VM:
	•	From the Vertex AI Workbench page, click “SSH” next to your instance.
	2.	Update Packages:

sudo apt update


	3.	Install XFCE and Additional Packages:

sudo apt install -y xfce4 xfce4-goodies

Install Chrome Remote Desktop

	1.	Download the Chrome Remote Desktop Debian Package:

wget https://dl.google.com/linux/direct/chrome-remote-desktop_current_amd64.deb


	2.	Install the Package:

sudo dpkg -i chrome-remote-desktop_current_amd64.deb


	3.	Install Dependencies:

sudo apt install -f -y

Install Stata 18 MP

	1.	Transfer the Stata Installer to the VM:
	•	Option 1: Use the “Upload file” feature in the SSH window.
	•	Option 2: Use scp from your local machine:

scp /path/to/stata18linux64.tar.gz [USERNAME]@[VM_EXTERNAL_IP]:~


	2.	Extract the Installer:

tar -xvzf stata18linux64.tar.gz


	3.	Run the Installer:

cd stata18linux64
sudo ./install

	•	Follow the on-screen prompts.
	•	Enter your license information when requested.

	4.	Verify Installation:

/usr/local/stata18/stata-mp

	•	Ensure that Stata launches without errors.

Configure Chrome Remote Desktop

	1.	Set Up the Desktop Environment:
	•	Create the session file for Chrome Remote Desktop:

echo "exec xfce4-session" > ~/.chrome-remote-desktop-session


	2.	Disable Light Locker (Optional but Recommended):
	•	Prevent screen locking issues:

sudo apt remove light-locker


	3.	Add User to Chrome Remote Desktop Group:

sudo usermod -a -G chrome-remote-desktop $USER


	4.	Enable and Start the Chrome Remote Desktop Service:

sudo systemctl enable chrome-remote-desktop@$USER
sudo systemctl start chrome-remote-desktop@$USER

Access Your VM via Chrome Remote Desktop

	1.	Set Up Remote Access:
	•	On your local machine, open Chrome Remote Desktop.
	•	Click “Set up remote access”.
	•	Under “Set up another computer”, click “Begin”.
	•	Choose “Linux” and copy the command provided.
	2.	Run the Command on the VM:
	•	Paste and execute the command in your SSH terminal.

DISPLAY= /opt/google/chrome-remote-desktop/start-host --code="YOUR_UNIQUE_CODE" --redirect-url="https://remotedesktop.google.com/_/oauthredirect" --name=$(hostname)


	•	Replace "YOUR_UNIQUE_CODE" with the code from the previous step.

	3.	Set a PIN:
	•	During execution, you’ll be prompted to enter and confirm a 6-digit PIN.
	•	This PIN will be used to authenticate when connecting remotely.
	4.	Access the VM:
	•	On your local machine, go to Chrome Remote Desktop Access.
	•	Your VM should appear under “Remote devices”.
	•	Click on it and enter your PIN to establish a remote desktop session.

Launch Stata 18 MP via Remote Desktop

	1.	Open Terminal in XFCE:
	•	Click on “Applications Menu” (top-left corner).
	•	Navigate to “Accessories” > “Terminal Emulator”.
	2.	Start Stata:

/usr/local/stata18/stata-mp

	•	Stata 18 MP should launch with its graphical interface.
	•	Optionally, create a desktop shortcut for quicker access:

cp /usr/local/stata18/stata.desktop ~/Desktop/
chmod +x ~/Desktop/stata.desktop

References

	•	Vertex AI Workbench Documentation
	•	Chrome Remote Desktop Help
	•	Stata Installation Guide for Unix
	•	XFCE Desktop Environment
	•	Google Cloud SCP Documentation

Note: Remember to stop or shut down your VM when not in use to prevent unnecessary charges. You can manage your instances from the GCP Console.
