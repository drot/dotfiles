#!/bin/bash

# Setup monitors
~/.screenlayout/primary.sh &

# Start compositing
picom --experimental-backends &

# Set wallpaper
~/.fehbg &

# Start NetworkManager applet
nm-applet &

# Start lock screen automatically
xautolock -time 1 -locker '~/.scripts/lock-screen' &

# Start WM
i3
