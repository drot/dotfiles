#!/bin/bash

# Reset NVIDIA
xrandr --setprovideroutputsource modesetting NVIDIA-0
xrandr --auto

# Setup monitors
xrandr --output eDP-1-1 --mode 1920x1080 --pos 889x1200 --rotate normal --output HDMI-1-1 --mode 1920x1200 --pos 1920x0 --rotate normal --output DP-1-1 --mode 1920x1200 --pos 0x0 --rotate normal

# Start compositing
picom --experimental-backends &

# Set wallpaper
~/.fehbg &

# Start WM
i3
