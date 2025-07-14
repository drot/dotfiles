#!/usr/bin/env bash
##
## File: /usr/local/bin/platinet-eth-reload.sh
## Purpose: Reload r8152 driver for Ethernet init
##

# Unload the Realtek Ethernet module
modprobe -r r8152

# Give it a quick moment to settle
sleep 0.5

# Reload the module
modprobe r8152
