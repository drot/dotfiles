#!/bin/bash

# Set wallpaper
xsetroot -bg '#333333' -mod 3 3

# Setup monitors
xrandr --output eDP-1 --primary --mode 1920x1080 --pos 974x1200 --rotate normal --output HDMI-1 --mode 1920x1200 --pos 1920x0 --rotate normal --output DP-1 --mode 1920x1200 --pos 0x0 --rotate normal

# Start WM
i3
