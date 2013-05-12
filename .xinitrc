#!/bin/sh

## Start session
systemd --user &

## Initialize defaults
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

## Disable power saving
xset -dpms &
xset s off &

## Start window manager
exec awesome
