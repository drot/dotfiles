#!/bin/sh

## Set X defaults
xrdb -merge ~/.Xresources

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

## Start window manager - awesome
exec ck-launch-session dbus-launch --sh-syntax --exit-with-session xmonad