#!/bin/bash
set -e

if [ $# -ne 1 ]; then
    echo "usage: $0 <device regex>"
    echo "Must be run outside the container in wsl."
    exit 1
fi

# Get the bus ID from the isbipd output.
BUSID=$(usbipd list | awk "match(\$0, /([1-9]*-[1-9]*).*$1/, out) { print out[1] }")

echo "Found device at bus id $BUSID"

usbipd bind --force -b $BUSID
usbipd attach --wsl -b $BUSID
