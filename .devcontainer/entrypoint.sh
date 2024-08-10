#!/bin/bash
set -e

# Postconfigure jlink.
cd /root && ./jlink.postinst

# Run the command passed to the entry point script.
exec $@
