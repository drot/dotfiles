#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# Initialize defaults
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

# Disable power saving
xset -dpms &
xset s off &

# Start rxvt-unicode daemon
urxvtd -q -f -o &

# Start awesome
exec awesome
