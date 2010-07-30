#!/bin/sh

sh ~/.fehbg &
xsetroot -cursor_name left_ptr &
xset -dpms &
urxvtd -q -o -f &
exec xmonad