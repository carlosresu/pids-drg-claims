#!/bin/bash

# Set partial window title to look for
WINDOW_MATCH="Program Error"

# Continuously scan every 0.5 seconds
while true; do
    # Try to find a window with the target name
    win_id=$(xdotool search --name "$WINDOW_MATCH" 2>/dev/null | head -n 1)

    if [ -n "$win_id" ]; then
        echo "Found popup. Sending 'i' to Ignore."
        xdotool windowactivate "$win_id"
        sleep 0.1
        xdotool key i
    fi

    sleep 0.5
done
