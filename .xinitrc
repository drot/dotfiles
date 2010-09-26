#!/bin/sh

xsetroot -cursor_name left_ptr &
xset -dpms &
sh ~/.fehbg &
urxvtd -q -o -f &
exec dbus-launch ck-launch-session xmonad
