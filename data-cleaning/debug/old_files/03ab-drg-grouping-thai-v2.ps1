# Get the absolute path to the current script and infer the repo root
$scriptPath = $MyInvocation.MyCommand.Path
$repoRoot = (Get-Item (Join-Path $scriptPath "..\..")).FullName

# Define important paths as pure strings
$exePath = "$repoRoot\TDRGv5\TGRP50V02.exe"
$inputDir = "$repoRoot\data-cleaning\data\chkpts\chkpt_4_thai_master_input"
$sandboxiePath = "C:\Program Files\Sandboxie-Plus\Start.exe"  # Adjust if installed elsewhere

# Define input files
$inputFiles = @(
  "chkpt_4_thai_grouper_input_2018_full_part_1_of_2.txt",
  "chkpt_4_thai_grouper_input_2018_full_part_2_of_2.txt",
  "chkpt_4_thai_grouper_input_2019_full_part_1_of_2.txt",
  "chkpt_4_thai_grouper_input_2019_full_part_2_of_2.txt",
  "chkpt_4_thai_grouper_input_2020_full_part_1_of_2.txt",
  "chkpt_4_thai_grouper_input_2020_full_part_2_of_2.txt",
  "chkpt_4_thai_grouper_input_2021_full_part_1_of_1.txt",
  "chkpt_4_thai_grouper_input_2022_full_part_1_of_2.txt",
  "chkpt_4_thai_grouper_input_2022_full_part_2_of_2.txt",
  "chkpt_4_thai_grouper_input_2023_full_part_1_of_2.txt",
  "chkpt_4_thai_grouper_input_2023_full_part_2_of_2.txt"
)

# Loop through files and launch them inside their respective sandbox
for ($i = 0; $i -lt $inputFiles.Count; $i++) {
  $boxName = "{0:D2}" -f ($i + 1)
  $inputFile = $inputFiles[$i]
  $inputPath = "$inputDir\$inputFile"

  # Validate file exists
  if (-Not (Test-Path $inputPath)) {
    Write-Warning "Missing input file: $inputPath"
    continue
  }

  Write-Host "Launching: $inputFile in Sandbox '$boxName'"

  # Launch with fully quoted, pure string paths (no PowerShell objects)
  Start-Process -NoNewWindow -FilePath "$sandboxiePath" `
    -ArgumentList @("/box:$boxName", "`"$exePath`"", "`"$inputPath`"")
}
