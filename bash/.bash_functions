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

    case $1 in
        -f)
            curl_opts="file=@$2"
            ;;
        -u)
            curl_opts="url=$2"
            ;;
        -s)
            curl_opts="shorten=$2"
            ;;
        *)
            echo "'-f' for file upload, '-u' for url upload, '-s' for URL shortening."
            return 1
    esac

    # Watch out if we're running X or not for clipboard pasting
    if [[ -z $DISPLAY ]]; then
        curl -# --fail -F "$curl_opts" "$url"
    else
        curl -# --fail -F "$curl_opts" "$url" | tr -d '\n' | xsel -b
    fi
}

# Record desktop
record () {
    if [[ $1 == *.mp4 ]]; then
        ffmpeg -y -f x11grab -s $(xdpyinfo | awk '/dimensions:/{print $2}') \
               -i :0.0 -f pulse -i 0 /tmp/"$1"
    else
        echo "Specify an .mp4 output file please."
        return 1
    fi
}
