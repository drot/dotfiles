#!/bin/bash

# Man page colorization
man () {
    LESS_TERMCAP_mb=$'\e[01;31m' \
    LESS_TERMCAP_md=$'\e[01;32m' \
    LESS_TERMCAP_me=$'\e[0m' \
    LESS_TERMCAP_se=$'\e[0m' \
    LESS_TERMCAP_so=$'\e[1;37;41m' \
    LESS_TERMCAP_ue=$'\e[0m' \
    LESS_TERMCAP_us=$'\e[04;34m' \
    command man "$@"
}

# Paste to 0x0.st
0x0 () {
    if [ "$1" = "-u" ]; then
        curl -F "file=@$2" https://0x0.st
    elif [ "$1" = "-s" ]; then
        curl -F "shorten=$2" https://0x0.st
    else
        echo "Specify '-u' to upload an image, '-s' to shorten a URL."
    fi
}

# Paste to sprunge.us
sprunge () {
    curl -F 'sprunge=<-' http://sprunge.us
}

# Find process
pids () {
    pgrep -l "$1"
}
