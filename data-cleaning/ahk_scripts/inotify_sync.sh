#!/usr/bin/env bash
inotifywait -m -r -e modify,create,delete ~/pids-drg-claims/TDRGv5/ ~/pids-drg-claims/data-cleaning/data/chkpts/chkpt_4_thai_master_input/ |
while read path _ file; do
    rsync -av --delete ~/pids-drg-claims/TDRGv5/ "$HOME/drg-wine32/drive_c/Program Files/TDRGv5/"
    rsync -av --delete ~/pids-drg-claims/data-cleaning/data/chkpts/chkpt_4_thai_master_input/ "$HOME/drg-wine32/drive_c/users/resurreccion_cmc/Input/"
    echo "Synced after detecting change in $path$file"
done
