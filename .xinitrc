#!/bin/sh

## test for an existing bus daemon, just to be safe
if test -z "$DBUS_SESSION_BUS_ADDRESS" ; then
             ## if not found, launch a new one
    eval `dbus-launch --sh-syntax --exit-with-session`
    echo "D-Bus per-session daemon address is: $DBUS_SESSION_BUS_ADDRESS"
fi

xrdb -merge ~/.Xresources
xsetroot -cursor_name left_ptr &
xset -dpms &
urxvtd -q -o -f &
emacs --daemon &
exec ck-launch-session awesome