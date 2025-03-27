#!/bin/bash

# Define the log file path
LOG_FILE="$HOME/pids-drg-claims/mem_usage.log"

# Ensure the directory exists
mkdir -p "$(dirname "$LOG_FILE")"

# Function to get current memory usage in MB
get_memory_usage() {
    free -m | awk '/Mem:/ {print $3}'
}

# Initialize the highest recorded memory usage
if [ ! -f "$LOG_FILE" ]; then
    echo "0" > "$LOG_FILE"
fi

# Read the highest memory usage recorded so far
HIGHEST_MEM=$(cat "$LOG_FILE")

echo "Tracking highest memory usage since $(date)..."
echo "Initial highest memory usage: ${HIGHEST_MEM} MB"

while true; do
    # Get current memory usage
    CURRENT_MEM=$(get_memory_usage)

    # Check if current usage is higher than recorded
    if (( CURRENT_MEM > HIGHEST_MEM )); then
        HIGHEST_MEM=$CURRENT_MEM
        echo "$HIGHEST_MEM" > "$LOG_FILE"
        echo "$(date) - New highest memory usage recorded: ${HIGHEST_MEM} MB"
    fi

    # Refresh every 1 second
    sleep 1
done
