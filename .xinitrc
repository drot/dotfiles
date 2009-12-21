#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

sh ~/.fehbg &
xsetroot -cursor_name left_ptr &
exec ck-launch-session xmonad 
