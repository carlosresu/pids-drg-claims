# Prompt user for debug mode
$debugChoice = Read-Host "Enable debugFlag? [y/n]"
$debugFlag = ($debugChoice -eq 'y')

# Kill all running AutoHotkey and TGRP50V02 processes to avoid conflicts
Get-Process AutoHotkey64, TGRP50V02 -ErrorAction SilentlyContinue | ForEach-Object {
    Write-Host "Killing existing process with ID $($_.Id): $($_.Name)"
    Stop-Process -Id $_.Id -Force
}

# Define paths
$scriptPath = Split-Path -Parent $MyInvocation.MyCommand.Definition
$repoRoot = Resolve-Path "$scriptPath\.."

$chkpt4 = Join-Path $repoRoot 'data-cleaning\data\chkpts\chkpt_4_thai_master_input'
$chkpt5 = Join-Path $repoRoot 'data-cleaning\data\chkpts\chkpt_5_thai_master_output'
$sandboxiePath = 'C:\Program Files\Sandboxie-Plus\Start.exe'
$sbCtrlPath = 'C:\Program Files\Sandboxie-Plus\SbieCtrl.exe'
$sbIni = 'C:\Windows\Sandboxie.ini'
$exePath = Join-Path $repoRoot 'TDRGv5\TGRP50V02.exe'
$bucketPre = 'gs://pids-drg-data/data/phic/thai/pre'
$bucketPost = 'gs://pids-drg-data/data/phic/thai/post'

# Remove all [Box*] sandbox definitions from Sandboxie.ini safely
$sbLines = Get-Content -Path $sbIni
$filteredLines = @()
$inBoxSection = $false

foreach ($line in $sbLines) {
    if ($line -match '^\[Box[A-Z]{2}\]') {
        $inBoxSection = $true
        continue
    }
    if ($line -match '^\[.+\]' -and $inBoxSection) {
        $inBoxSection = $false
    }
    if (-not $inBoxSection) {
        $filteredLines += $line
    }
}

$filteredLines | Set-Content -Path $sbIni -Encoding ASCII
Write-Host "Removed all [Box*] sandbox definitions from Sandboxie.ini."

if (Test-Path $sbCtrlPath) {
    & "$sbCtrlPath" reload
    Start-Sleep -Seconds 1
    Write-Host "Reloaded Sandboxie configuration."
}

# Ensure input/output directories exist
New-Item -ItemType Directory -Force -Path $chkpt4 | Out-Null
New-Item -ItemType Directory -Force -Path $chkpt5 | Out-Null

# Optional cleanup of previous data
if ((Read-Host "Delete old input files in '$chkpt4'? [y/n]") -eq 'y') {
    Remove-Item "$chkpt4\*" -Force
}
if ((Read-Host "Delete old output files in '$chkpt5'? [y/n]") -eq 'y') {
    Remove-Item "$chkpt5\*" -Force
}

# Optional download from GCS bucket
if ((Read-Host "Download files from $bucketPre to $chkpt4? [y/n]") -eq 'y') {
    $pattern = if ($debugFlag) { "*of_1.txt" } else { "*" }
    & gsutil -m cp "$bucketPre/$pattern" "$chkpt4"
}
else {
    Write-Host "Skipping download."
}

# Remove temporary .gstmp files
Get-ChildItem "$chkpt4\*.gstmp" -ErrorAction SilentlyContinue | Remove-Item -Force

# Validate input files
$inputFiles = Get-ChildItem -Path $chkpt4 -File -Filter *.txt
if ($inputFiles.Count -eq 0) {
    Write-Host "No files to process."; exit
}

# Ensure all parts of each file group are present
$yearPartMap = @{}
foreach ($file in $inputFiles) {
    if ($file.Name -match '.*_(\d{4})_full_part_(\d+)_of_(\d+)\.txt') {
        $year, $part, $of = $Matches[1..3]
        $key = "$year-of-$of"
        if (-not $yearPartMap.ContainsKey($key)) { $yearPartMap[$key] = @{} }
        $yearPartMap[$key][$part] = $true
    }
}
foreach ($key in $yearPartMap.Keys) {
    $expected = [int]($key.Split('-')[2])
    $foundParts = $yearPartMap[$key].Keys | ForEach-Object { [int]$_ }
    for ($i = 1; $i -le $expected; $i++) {
        if (-not ($foundParts -contains $i)) {
            Write-Host "Missing part $i of $expected for $key. Aborting."; exit
        }
    }
}

# Generate unique Box names: BoxAA, BoxAB, etc.
function Get-BoxName($index) {
    $alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    $first = [math]::Floor($index / 26)
    $second = $index % 26
    return "Box" + $alphabet[$first] + $alphabet[$second]
}

# Generate dynamic sandbox configurations
$sbDynamicFile = "$env:TEMP\sb_dynamic_sections.ini"
Remove-Item $sbDynamicFile -ErrorAction SilentlyContinue
Add-Content $sbDynamicFile "" -Encoding ASCII

for ($i = 0; $i -lt $inputFiles.Count; $i++) {
    $box = Get-BoxName $i
    Add-Content $sbDynamicFile @(
        "[$box]",
        "Enabled=y",
        "OpenFilePath=$chkpt4",
        "OpenFilePath=$chkpt5",
        "BlockNetworkFiles=y",
        "BorderColor=#00FFFF,ttl",
        "Template=OpenBluetooth",
        "Template=SkipHook",
        "Template=FileCopy",
        "Template=qWave",
        "Template=BlockPorts",
        "Template=LingerPrograms",
        "Template=AutoRecoverIgnore",
        "ConfigLevel=10",
        "UseFileDeleteV2=y",
        "UseRegDeleteV2=y",
        "AutoRecover=y",
        ""
    ) -Encoding ASCII
}

# Append dynamic config to Sandboxie.ini and reload
Copy-Item -Path $sbIni -Destination "$sbIni.bak" -Force
Get-Content $sbDynamicFile | Add-Content -Path $sbIni -Encoding ASCII
if (Test-Path $sbCtrlPath) { & "$sbCtrlPath" reload }
Start-Sleep -Seconds 2

# Launch TGRP50V02.exe for each input file in a sandbox
for ($i = 0; $i -lt $inputFiles.Count; $i++) {
    $file = $inputFiles[$i].FullName
    $box = Get-BoxName $i
    Write-Host "Launching: $exePath '$file' in $box"
    Start-Process -FilePath $sandboxiePath -ArgumentList "/box:$box", "\"$exePath\"", "\"$file\""
}

# Function to wait for sandboxed processes to finish
function Wait-AllSandboxes {
    $ahkScript = Join-Path $repoRoot 'data-cleaning\ahk_scripts\ignore_numeric_overflows.ahk'
    $ahkExe = "C:\Program Files\AutoHotkey\v2\AutoHotkey64.exe"

    if ((Test-Path $ahkExe) -and (Test-Path $ahkScript)) {
        Write-Host "Launching AutoHotkey script to suppress numeric overflow popups..."
        Start-Process -FilePath $ahkExe -ArgumentList "\"$ahkScript\"" -WindowStyle Hidden
    }
    else {
        Write-Warning "AutoHotkey.exe or script not found."
    }

    while ($true) {
        $anonCount = Get-WmiObject Win32_Process | ForEach-Object {
            try {
                $owner = $_.GetOwner()
                if ($owner.User -eq 'ANONYMOUS LOGON') { $_ }
            }
            catch {}
        } | Measure-Object | Select-Object -ExpandProperty Count

        if ($anonCount -eq 0) { break }
        Start-Sleep -Seconds 2
    }

    # Clean up AHK process after processing
    Get-Process AutoHotkey64 -ErrorAction SilentlyContinue | ForEach-Object {
        Stop-Process -Id $_.Id -Force
    }
}

Wait-AllSandboxes

# Move processed output files to output checkpoint folder
Get-ChildItem "$chkpt4\*Res.TXT" -ErrorAction SilentlyContinue | Move-Item -Destination $chkpt5 -Force

# Validate output completeness
$outputFiles = Get-ChildItem "$chkpt5\*Res.TXT"
$yearPartMap.Clear()
foreach ($file in $outputFiles) {
    if ($file.Name -match '.*_(\d{4})_full_part_(\d+)_of_(\d+)_Res\.TXT') {
        $year, $part, $of = $Matches[1..3]
        $key = "$year-of-$of"
        if (-not $yearPartMap.ContainsKey($key)) { $yearPartMap[$key] = @{} }
        $yearPartMap[$key][$part] = $true
    }
}
foreach ($key in $yearPartMap.Keys) {
    $expected = [int]($key.Split('-')[2])
    $foundParts = $yearPartMap[$key].Keys | ForEach-Object { [int]$_ }
    for ($i = 1; $i -le $expected; $i++) {
        if (-not ($foundParts -contains $i)) {
            Write-Host "Missing output part $i of $expected for $key. Aborting."; exit
        }
    }
}

# Upload output files to GCS
& gsutil -m cp -r "$chkpt5\*" "$bucketPost/"
Write-Host "All steps completed."
Read-Host "Press any key to exit"