# Prompt debug flag early
$debugChoice = Read-Host "Enable debugFlag? [y/n]"
$debugFlag = ($debugChoice -eq 'y')

# Kill all running AutoHotkey processes at the start to ensure a clean state
Get-Process AutoHotkey64 -ErrorAction SilentlyContinue | ForEach-Object {
    Write-Host "Killing existing AutoHotkey process with ID $($_.Id)"
    Stop-Process -Id $_.Id -Force
}

Get-Process TGRP50V02 -ErrorAction SilentlyContinue | ForEach-Object {
    Write-Host "Killing existing AutoHotkey process with ID $($_.Id)"
    Stop-Process -Id $_.Id -Force
}

# Set script directory
$scriptPath = Split-Path -Parent $MyInvocation.MyCommand.Definition
$repoRoot = Resolve-Path "$scriptPath\.."

# Paths
$chkpt4 = Join-Path $repoRoot 'data-cleaning\data\chkpts\chkpt_4_thai_master_input'
$chkpt5 = Join-Path $repoRoot 'data-cleaning\data\chkpts\chkpt_5_thai_master_output'
$sandboxiePath = 'C:\Program Files\Sandboxie-Plus\Start.exe'
$sbCtrlPath = 'C:\Program Files\Sandboxie-Plus\SbieCtrl.exe'
$sbIni = 'C:\Windows\Sandboxie.ini'
$exePath = Join-Path $repoRoot 'TDRGv5\TGRP50V02.exe'
$bucketPre = 'gs://pids-drg-data/data/phic/thai/pre'
$bucketPost = 'gs://pids-drg-data/data/phic/thai/post'

# Ensure directories exist
New-Item -ItemType Directory -Force -Path $chkpt4 | Out-Null
New-Item -ItemType Directory -Force -Path $chkpt5 | Out-Null

# Delete input/output
if ((Read-Host "Delete old input files in '$chkpt4'? [y/n]") -eq 'y') { Remove-Item "$chkpt4\*" -Force }
if ((Read-Host "Delete old output files in '$chkpt5'? [y/n]") -eq 'y') { Remove-Item "$chkpt5\*" -Force }

# Download files
if ((Read-Host "Download files from $bucketPre to $chkpt4? [y/n]") -eq 'y') {
    $pattern = if ($debugFlag) { "*of_1.txt" } else { "*" }
    & gsutil -m cp "$bucketPre/$pattern" "$chkpt4"
}
else {
    Write-Host "Skipping download."
}

# Remove temp files
Get-ChildItem "$chkpt4\*.gstmp" -ErrorAction SilentlyContinue | Remove-Item -Force

# Input file validation
$inputFiles = Get-ChildItem -Path $chkpt4 -File -Filter *.txt
if ($inputFiles.Count -eq 0) { Write-Host "No files to process."; exit }

$yearPartMap = @{}
foreach ($file in $inputFiles) {
    if ($file.Name -match '.*_(\d{4})_full_part_(\d+)_of_(\d+)\.txt') {
        $year, $part, $of = $Matches[1..3]
        $key = "$year-of-$of"
        if (-not $yearPartMap.ContainsKey($key)) {
            $yearPartMap[$key] = @{}
        }
        $yearPartMap[$key][$part] = $true
    }
}

foreach ($key in $yearPartMap.Keys) {
    $expected = [int]($key.Split('-')[2])
    $foundParts = $yearPartMap[$key].Keys | ForEach-Object { [int]$_ }
    for ($i = 1; $i -le $expected; $i++) {
        if (-not ($foundParts -contains $i)) {
            Write-Host "Missing part $i of $expected for $key. Aborting."
            exit
        }
    }
}

# Box name generator
function Get-BoxName($index) {
    $alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    $first = [math]::Floor($index / 26)
    $second = $index % 26
    return "Box" + $alphabet[$first] + $alphabet[$second]
}

# Read all lines of Sandboxie.ini
$sbLines = Get-Content -Path $sbIni

# Initialize variables
$filteredLines = @()
$inBoxSection = $false

foreach ($line in $sbLines) {
    # Check if this line starts a [Box*] section
    if ($line -match '^\[Box[A-Z]{2}\]') {
        $inBoxSection = $true
        continue  # Skip header line of Box section
    }

    # If we encounter a new section and we're currently in a [Box*] section, exit it
    if ($line -match '^\[.+\]' -and $inBoxSection) {
        $inBoxSection = $false
    }

    # Only include the line if not in a [Box*] section
    if (-not $inBoxSection) {
        $filteredLines += $line
    }
}

# Overwrite the ini file with the filtered lines using ASCII to avoid corruption
$filteredLines | Set-Content -Path $sbIni -Encoding ASCII
Write-Host "Removed all [Box*] sandbox definitions from Sandboxie.ini."

# Reload the Sandboxie configuration
if (Test-Path $sbCtrlPath) {
    & "$sbCtrlPath" reload
    Start-Sleep -Seconds 1
    Write-Host "Reloaded Sandboxie configuration after cleanup."
}

# Sandbox config
$sbDynamicFile = "$env:TEMP\sb_dynamic_sections.ini"
Remove-Item $sbDynamicFile -ErrorAction SilentlyContinue
Add-Content $sbDynamicFile ""

for ($i = 0; $i -lt $inputFiles.Count; $i++) {
    $box = Get-BoxName $i
    Add-Content $sbDynamicFile "[$box]"
    Add-Content $sbDynamicFile "Enabled=y"
    Add-Content $sbDynamicFile "OpenFilePath=$chkpt4"
    Add-Content $sbDynamicFile "OpenFilePath=$chkpt5"
    Add-Content $sbDynamicFile "BlockNetworkFiles=y"
    Add-Content $sbDynamicFile "BorderColor=#00FFFF,ttl"
    Add-Content $sbDynamicFile "Template=OpenBluetooth"
    Add-Content $sbDynamicFile "Template=SkipHook"
    Add-Content $sbDynamicFile "Template=FileCopy"
    Add-Content $sbDynamicFile "Template=qWave"
    Add-Content $sbDynamicFile "Template=BlockPorts"
    Add-Content $sbDynamicFile "Template=LingerPrograms"
    Add-Content $sbDynamicFile "Template=AutoRecoverIgnore"
    Add-Content $sbDynamicFile "ConfigLevel=10"
    Add-Content $sbDynamicFile "UseFileDeleteV2=y"
    Add-Content $sbDynamicFile "UseRegDeleteV2=y"
    Add-Content $sbDynamicFile "AutoRecover=y"
    Add-Content $sbDynamicFile ""
}

Copy-Item -Path $sbIni -Destination "$sbIni.bak" -Force
Get-Content $sbDynamicFile | Add-Content -Path $sbIni -Encoding ASCII
if (Test-Path $sbCtrlPath) { & "$sbCtrlPath" reload }
Start-Sleep -Seconds 2

# Launch
for ($i = 0; $i -lt $inputFiles.Count; $i++) {
    $file = $inputFiles[$i].FullName
    $box = Get-BoxName $i
    Write-Host "Launching: $exePath `"$file`" in $box"
    Start-Process -FilePath $sandboxiePath -ArgumentList "/box:$box", "`"$exePath`"", "`"$file`""
}

# Wait
function Wait-AllSandboxes {
    # Construct path to the AHK script based on your repo layout
    $ahkScript = Join-Path $repoRoot 'data-cleaning\ahk_scripts\ignore_numeric_overflows.ahk'

    # Try to locate AutoHotkey.exe - update this path if needed
    $ahkExe = "C:\Program Files\AutoHotkey\v2\AutoHotkey64.exe"

    # Launch AHK script if both paths exist
    if ((Test-Path $ahkExe) -and (Test-Path $ahkScript)) {
        Write-Host "Launching AutoHotkey script to suppress numeric overflow popups..."
        Start-Process -FilePath $ahkExe -ArgumentList "`"$ahkScript`"" -WindowStyle Hidden
    }
    else {
        Write-Warning "AutoHotkey.exe or AHK script not found."
        Write-Host "  AutoHotkey: $ahkExe"
        Write-Host "  AHK Script: $ahkScript"
    }

    # Monitor sandbox processes owned by ANONYMOUS LOGON
    while ($true) {
        $anonCount = Get-WmiObject Win32_Process |
        ForEach-Object {
            try {
                $owner = $_.GetOwner()
                if ($owner.User -eq 'ANONYMOUS LOGON') { $_ }
            }
            catch {}
        } | Measure-Object | Select-Object -ExpandProperty Count

        if ($anonCount -eq 0) { break }
        Start-Sleep -Seconds 2
    }

    # Kill AutoHotkey once done
    Get-Process AutoHotkey -ErrorAction SilentlyContinue | ForEach-Object {
        Stop-Process -Id $_.Id -Force
    }
}

Wait-AllSandboxes

# Move outputs
Get-ChildItem "$chkpt4\*Res.TXT" -ErrorAction SilentlyContinue | Move-Item -Destination $chkpt5 -Force

# Output file validation
$outputFiles = Get-ChildItem "$chkpt5\*Res.TXT"
$yearPartMap.Clear()
foreach ($file in $outputFiles) {
    if ($file.Name -match '.*_(\d{4})_full_part_(\d+)_of_(\d+)_Res\.TXT') {
        $year, $part, $of = $Matches[1..3]
        $key = "$year-of-$of"
        if (-not $yearPartMap.ContainsKey($key)) {
            $yearPartMap[$key] = @{}
        }
        $yearPartMap[$key][$part] = $true
    }
}

foreach ($key in $yearPartMap.Keys) {
    $expected = [int]($key.Split('-')[2])
    $foundParts = $yearPartMap[$key].Keys | ForEach-Object { [int]$_ }
    for ($i = 1; $i -le $expected; $i++) {
        if (-not ($foundParts -contains $i)) {
            Write-Host "Missing output part $i of $expected for $key. Aborting."
            exit
        }
    }
}

# Upload
& gsutil -m cp -r "$chkpt5\*" "$bucketPost/"
Write-Host "All steps completed."
Read-Host "Press any key to exit"
