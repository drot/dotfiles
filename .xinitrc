#!/bin/sh

xsetroot -cursor_name left_ptr &
xset -dpms &
urxvtd -q -o -f &
emacs --daemon &
exec dbus-launch ck-launch-session awesome