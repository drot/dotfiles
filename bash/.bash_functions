#!/bin/bash

# Man page colorization
man() {
    LESS_TERMCAP_mb=$'\e[01;31m' \
    LESS_TERMCAP_md=$'\e[01;32m' \
    LESS_TERMCAP_me=$'\e[0m' \
    LESS_TERMCAP_se=$'\e[0m' \
    LESS_TERMCAP_so=$'\e[01;37;41m' \
    LESS_TERMCAP_ue=$'\e[0m' \
    LESS_TERMCAP_us=$'\e[03;04;34m' \
    command man "$@"
}

# Paste to 0x0.st
0x0() {
    local curl_opts
    local url="https://0x0.st"
    # Parameter pick
    if [[ $1 = "-f" ]]; then
        curl_opts="file=@$2 $url"
    elif [[ $1 = "-u" ]]; then
        curl_opts="url=$2 $url"
    elif [[ $1 = "-s" ]]; then
        curl_opts="shorten=$2 $url"
    else
        echo "'-f' for file upload, '-u' for url upload, '-s' for URL shortening."
        return 1
    fi
    # Execute upload and send to clipboard
    curl -# -F $curl_opts | tr -d '\n' | xsel
}

# Find process
pids() {
    pgrep -l "$1"
}
