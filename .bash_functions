#!/bin/bash

# Paste to sprunge
sprunge() {
    curl -F 'sprunge=<-' http://sprunge.us
}

# Watch online videos with MPlayer
tube() {
    youtube-dl -q -o- $1 | mplayer -cache 8192 -
}
