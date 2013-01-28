#!/bin/sh

## Initialize defaults
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

## Mouse cursor
xsetroot -cursor_name left_ptr &

## Set background
sh ~/.fehbg &

## Disable power saving
xset -dpms &
xset s off &

## Launch rxvt-unicode daemon
urxvtd -q -o -f &

## Launch Emacs daemon
emacs --daemon &

## Start window manager
exec i3
