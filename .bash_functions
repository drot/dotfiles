#!/bin/bash

# Send to pastebin
sprunge() {
    cat $1 | curl -F 'sprunge=<-' http://sprunge.us
}

# Watch online videos with MPlayer
function ytwatch() {
    youtube-dl -q -o- $1 | mplayer -cache 8192 -
}
