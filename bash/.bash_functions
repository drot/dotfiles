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
    # Execute upload
    curl $opts | xclip
}

# Paste to ix.io
ix() {
    local opts
    local OPTIND
    [ -f "$HOME/.netrc" ] && opts='-n'
    while getopts ":hd:i:n:" x; do
        case $x in
            h) echo "ix [-d ID] [-i ID] [-n N] [opts]"; return;;
            d) $echo curl $opts -X DELETE ix.io/$OPTARG; return;;
            i) opts="$opts -X PUT"; local id="$OPTARG";;
            n) opts="$opts -F read:1=$OPTARG";;
        esac
    done
    shift $(($OPTIND - 1))
    [ -t 0 ] && {
        local filename="$1"
        shift
        [ "$filename" ] && {
            curl $opts -F f:1=@"$filename" $* ix.io/$id
            return
        }
        echo "^C to cancel, ^D to send."
    }
    curl $opts -F f:1='<-' $* ix.io/$id
}

# Find process
pids() {
    pgrep -l "$1"
}
