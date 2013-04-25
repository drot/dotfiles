#!/bin/sh

## Start session
systemd --user &

## Initialize defaults
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

## Mouse cursor
xsetroot -cursor_name left_ptr &

## Disable power saving
xset -dpms &
xset s off &

## Start window manager
exec awesome
