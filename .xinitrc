#!/bin/sh

## Mouse cursor
xsetroot -cursor_name left_ptr &

## Set background
sh ~/.fehbg

## Disable power saving
xset -dpms &

## Launch rxvt-unicode daemon
urxvtd -q -o -f &

## Launch Emacs daemon
emacs --daemon &

## Start window manager
exec ck-launch-session dbus-launch --sh-syntax --exit-with-session xmonad
