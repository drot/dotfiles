#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

xsetroot -cursor_name left_ptr &
sh ~/.fehbg &
urxvtd -q -f -o &
exec xmonad
