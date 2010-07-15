#!/bin/sh

xsetroot -cursor_name left_ptr &
sh ~/.fehbg &
xset -dpms &
urxvtd -q -f -o &
xmonad
