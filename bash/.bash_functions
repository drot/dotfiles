#!/bin/bash

# Paste to 0x0.st
function 0x0 {
    if [ "$1" = "-u" ]; then
        curl -F "file=@$2" https://0x0.st
    elif [ "$1" = "-s" ]; then
        curl -F "shorten=$2" https://0x0.st
    else
        echo "Enter '-u' to upload an image, '-s' to shorten a URL."
    fi
}

# Paste to sprunge.us
function sprunge {
    curl -F 'sprunge=<-' http://sprunge.us
}

# Find process
function pids {
    ps aux | grep -i $1
}
