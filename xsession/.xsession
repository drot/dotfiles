#!/bin/bash

# Reset NVIDIA
xrandr --setprovideroutputsource modesetting NVIDIA-0
xrandr --auto

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
