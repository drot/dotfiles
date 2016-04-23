#!/bin/bash

# Watch online videos with MPlayer
function ytwatch() {
    youtube-dl -q -o- $1 | mplayer -cache 8192 -
}
