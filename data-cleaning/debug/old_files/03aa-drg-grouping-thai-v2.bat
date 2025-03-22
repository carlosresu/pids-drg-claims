@echo off
setlocal enabledelayedexpansion

:: --- Set base path to script location ---
cd /d "%~dp0"

:: --- Paths and Config ---
set "CHKPT4=data-cleaning\data\chkpts\chkpt_4_thai_master_input"
set "CHKPT5=data-cleaning\data\chkpts\chkpt_5_thai_output"
set "BUCKET=gs://pids-drg-data/data/phic/thai/pre"

:: --- List of input files ---
set FILES=chkpt_4_thai_grouper_input_2018_full_part_1_of_2.txt
set FILES=!FILES! chkpt_4_thai_grouper_input_2018_full_part_2_of_2.txt
set FILES=!FILES! chkpt_4_thai_grouper_input_2019_full_part_1_of_2.txt
set FILES=!FILES! chkpt_4_thai_grouper_input_2019_full_part_2_of_2.txt
set FILES=!FILES! chkpt_4_thai_grouper_input_2020_full_part_1_of_2.txt
set FILES=!FILES! chkpt_4_thai_grouper_input_2020_full_part_2_of_2.txt
set FILES=!FILES! chkpt_4_thai_grouper_input_2021_full_part_1_of_1.txt
set FILES=!FILES! chkpt_4_thai_grouper_input_2022_full_part_1_of_2.txt
set FILES=!FILES! chkpt_4_thai_grouper_input_2022_full_part_2_of_2.txt
set FILES=!FILES! chkpt_4_thai_grouper_input_2023_full_part_1_of_2.txt
set FILES=!FILES! chkpt_4_thai_grouper_input_2023_full_part_2_of_2.txt

:: --- Check if gcloud is installed ---
where gcloud >nul 2>&1
if errorlevel 1 (
    echo ❌ gcloud not found.
    echo Please install gcloud CLI:
    echo curl -o gcloud_installer.exe https://dl.google.com/dl/cloudsdk/channels/rapid/GoogleCloudSDKInstaller.exe
    echo start gcloud_installer.exe
    echo gcloud init
    pause
) else (
    echo ✅ gcloud CLI found.
    echo If not yet initialized, run: gcloud init
    pause
)

:: --- Ensure input/output directories exist ---
if not exist "%CHKPT4%" (
    mkdir "%CHKPT4%"
)
if not exist "%CHKPT5%" (
    mkdir "%CHKPT5%"
)

:: --- Download input files ---
echo 📥 Downloading input files from GCS...
set "CMD=gsutil -m cp"
for %%F in (!FILES!) do (
    set "CMD=!CMD! %BUCKET%/%%F"
)
set "CMD=!CMD! %CHKPT4%"
call !CMD!

:: --- Wait for Thai Grouper ---
set /p CONFIRM=🚨 Run the Thai Batch Grouper manually, then type y to continue: 
if /i not "!CONFIRM!"=="y" (
    echo ❌ Grouper not confirmed as complete. Aborting.
    exit /b 1
)

:: --- Upload output files ---
echo 📤 Uploading output files to GCS...
gsutil -m cp -r "%CHKPT5%\*" gs://pids-drg-data/data/phic/thai/post/

echo ✅ Done!
pause
