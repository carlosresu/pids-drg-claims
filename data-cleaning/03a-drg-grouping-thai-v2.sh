#!/bin/bash

set -e

# Define paths
REPO_ROOT="$HOME/drg-pipeline"
INPUT_DIR="$REPO_ROOT/data-cleaning/data/chkpts/chkpt_4_thai_master_input"
EXE_PATH="$REPO_ROOT/TDRGv5/TGRP50V02.exe"
IGNORE_SCRIPT="$REPO_ROOT/data-cleaning/ahk_scripts/ignore_numeric_overflows.sh"
WINEPREFIX="$HOME/drg-wine32"
export WINEPREFIX
export WINEDEBUG=-all

# Ensure 32-bit WINEPREFIX exists
if [ ! -d "$WINEPREFIX" ]; then
    echo "Creating 32-bit WINEPREFIX at $WINEPREFIX"
    WINEARCH=win32 wineboot
fi

# Disable Wine crash dialogs
echo "Disabling Wine crash dialog popups..."
wine reg add "HKCU\\Software\\Wine\\WineDbg" /v ShowCrashDialog /t REG_DWORD /d 0 /f

# Validate EXE path
if [ ! -f "$EXE_PATH" ]; then
    echo "❌ EXE not found at $EXE_PATH"
    exit 1
fi

# Start popup suppressor if available
if [ -f "$IGNORE_SCRIPT" ]; then
    echo "Launching overflow suppressor..."
    bash "$IGNORE_SCRIPT" &
    IGNORE_PID=$!
fi

# Find all input files containing '_full_'
mapfile -t FILES < <(find "$INPUT_DIR" -type f -name "*_full_*.txt" | sort)

if [ ${#FILES[@]} -eq 0 ]; then
    echo "❌ No matching input files found."
    exit 1
fi

echo "🚀 Launching grouper for ${#FILES[@]} input files..."

# Track background PIDs
PIDS=()

# Spawn Wine+Firejail process per file
for FILE in "${FILES[@]}"; do
    echo "Launching: $(basename "$FILE")"
    firejail --noprofile --net=none \
        --whitelist="$WINEPREFIX" \
        --whitelist="$(dirname "$EXE_PATH")" \
        --whitelist="$(dirname "$FILE")" \
        --whitelist="$HOME" \
        env WINEPREFIX="$WINEPREFIX" \
        wine "$EXE_PATH" "$(WINEPREFIX="$WINEPREFIX" winepath -w "$FILE")" &
    PIDS+=($!)
done

# Wait for all launched jobs
for pid in "${PIDS[@]}"; do
    wait "$pid"
done

echo "🎉 All grouper jobs completed."

# Kill suppressor if running
if [ -n "$IGNORE_PID" ]; then
    echo "Stopping overflow suppressor..."
    kill "$IGNORE_PID"
fi

echo "✅ Done."
