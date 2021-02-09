#!/bin/bash

# Source bash defaults
if [[ -f /etc/profile ]]; then
    . /etc/profile
fi

# Setup monitors
~/.screenlayout/primary.sh &

# Start compositing
picom --experimental-backends &

# Set wallpaper
~/.fehbg &

# Start NetworkManager applet
nm-applet &

# Start WM
i3
