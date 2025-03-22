#!/usr/bin/env bash
inotifywait -m -r -e modify,create,delete ~/drg-pipeline/TDRGv5/ ~/drg-pipeline/data-cleaning/data/chkpts/chkpt_4_thai_master_input/ |
while read path _ file; do
    rsync -av --delete ~/drg-pipeline/TDRGv5/ "$HOME/drg-wine32/drive_c/Program Files/TDRGv5/"
    rsync -av --delete ~/drg-pipeline/data-cleaning/data/chkpts/chkpt_4_thai_master_input/ "$HOME/drg-wine32/drive_c/users/resurreccion_cmc/Input/"
    echo "Synced after detecting change in $path$file"
done
