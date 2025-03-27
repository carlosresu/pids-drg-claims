#!/bin/bash

# --------------------------------------------------------
# 1. SETUP
# --------------------------------------------------------
WINEPREFIX="$HOME/drg-wine32"
EXE_WIN="C:\\\\Program Files\\\\TDRGv5\\\\TGRP50V02.exe"
INPUT_DIR="$HOME/drg-wine32/drive_c/users/resurreccion_cmc/Input"
SUPPRESSOR="$HOME/pids-drg-claims/data-cleaning/ahk_scripts/ignore_numeric_overflows.sh"

# --------------------------------------------------------
# 2. OPTIONAL: LAUNCH SUPPRESSOR
# --------------------------------------------------------
if [[ -f "$SUPPRESSOR" ]]; then
    echo "🔄 Starting overflow suppressor..."
    bash "$SUPPRESSOR" &
    SUPPRESSOR_PID=$!
fi

# --------------------------------------------------------
# 3. FIND INPUT FILES
# --------------------------------------------------------
# Collect all .txt files matching '*_full_*.txt' inside Input folder
mapfile -t input_files < <(find "$INPUT_DIR" -type f -name '*_full_*.txt' | sort)

if [[ ${#input_files[@]} -eq 0 ]]; then
    echo "❌ No matching *_full_*.txt files found in $INPUT_DIR"
    exit 1
fi

# --------------------------------------------------------
# 4. LAUNCH PROCESSES
# --------------------------------------------------------
for f in "${input_files[@]}"; do
    filename=$(basename "$f")
    # Convert to Windows path "C:\users\resurreccion_cmc\Input\filename.txt"
    wine_input_path="C:\\\\users\\\\resurreccion_cmc\\\\Input\\\\${filename//\//\\\\}"

    echo "🚀 Launching: $filename"

    firejail --noprofile --net=none \
      --whitelist="$HOME/drg-wine32" \
      env WINEPREFIX="$WINEPREFIX" \
      wine "$EXE_WIN" "$wine_input_path" &
done

# --------------------------------------------------------
# 5. WAIT FOR ALL BACKGROUND PROCESSES
# --------------------------------------------------------
wait
echo "✅ All DRG processes completed."

# --------------------------------------------------------
# 6. STOP SUPPRESSOR (IF ANY)
# --------------------------------------------------------
if [[ -n "$SUPPRESSOR_PID" ]]; then
    echo "🛑 Stopping overflow suppressor..."
    kill "$SUPPRESSOR_PID"
fi

echo "🎉 Done."
