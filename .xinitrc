#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

xsetroot -cursor_name left_ptr &
sh ~/.fehbg &
[[ ! -r "/tmp/emacs${UID}/server" ]] && emacs --daemon &
urxvtd -q -f -o &
exec ck-launch-session xmonad
