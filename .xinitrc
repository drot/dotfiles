#!/bin/sh

xsetroot -cursor_name left_ptr &
sh ~/.fehbg &
xset -dpms &
xrdb $HOME/.Xresources &
urxvtd -q -f -o &
exec xmonad