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
        curl_opts="file=@$2"
    elif [[ $1 = "-u" ]]; then
        curl_opts="url=$2"
    elif [[ $1 = "-s" ]]; then
        curl_opts="shorten=$2"
    else
        echo "'-f' for file upload, '-u' for url upload, '-s' for URL shortening."
        return 1
    fi
    # Upload command
    local upload="curl -# -F $curl_opts $url"
    
    # Watch out if we're running X or not for clipboard pasting
    if [[ -z $DISPLAY ]]; then
        $upload
    else
        $upload | tr -d '\n' | xsel
    fi
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
