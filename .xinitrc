#!/bin/sh

## Start session
/usr/lib/systemd/systemd --user &

## Initialize defaults
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

## Disable power saving
xset -dpms &
xset s off &

## Start window manager
exec awesome
