#!/bin/sh

## D-Bus session start
if test -z "$DBUS_SESSION_BUS_ADDRESS" ; then
    ## if not found, launch a new one
    eval `dbus-launch --sh-syntax --exit-with-session`
    echo "D-Bus per-session daemon address is: $DBUS_SESSION_BUS_ADDRESS"
fi

## Set X defaults
xrdb -merge ~/.Xresources

## Mouse cursor
xsetroot -cursor_name left_ptr &

## Disable power saving
xset -dpms &

## Launch rxvt-unicode daemon
urxvtd -q -o -f &

## Launch Emacs daemon
emacs --daemon &

## Start window manager - awesome
exec ck-launch-session awesome