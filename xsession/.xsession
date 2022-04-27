#!/bin/bash

# Setup monitors
autorandr --cycle &

# Start compositing
picom --experimental-backends &

# Start NetworkManager applet
nm-applet &

# Start lock screen automatically
xautolock &

# Start WM
i3
