#!/bin/bash

# Man page colorization
man() {
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
0x0() {
    local opts
    local URL="https://0x0.st"
    # Parameter pick
    if [ "$1" = "-f" ]; then
        opts="-F file=@$2 $URL"
    elif [ "$1" = "-u" ]; then
        opts="-F url=$2 $URL"
    elif [ "$1" = "-s" ]; then
        opts="-F shorten=$2 $URL"
    else
        echo "'-f' for file upload, '-u' for URL upload, '-s' for URL shortening."
        return
    fi
    # Execute upload and send to clipboard
    curl $opts | xclip
}

# Find process
pids() {
    pgrep -l "$1"
}
