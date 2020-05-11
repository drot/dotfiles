#!/bin/bash

# Man page colorization
man () {
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
0x0 () {
    local curl_opts
    local url="https://0x0.st"

    # Parameter pick
    case $1 in
        -f)
            : "file=@$2"
            ;;
        -u)
            : "url=$2"
            ;;
        -s)
            : "shorten=$2"
            ;;
        *)
            echo "'-f' for file upload, '-u' for url upload, '-s' for URL shortening."
            return 1
    esac

    curl_opts="$_"

    # Watch out if we're running X or not for clipboard pasting
    if [[ -z $DISPLAY ]]; then
        curl -# -F "$curl_opts" "$url"
    else
        curl -# -F "$curl_opts" "$url" | tr -d '\n' | xsel -b
    fi
}
