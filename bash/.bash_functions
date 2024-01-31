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

    upload_action () {
        curl -# --fail -F "$curl_opts" "$url"
    }

    # Watch out if we're running X or not for clipboard pasting
    if [[ -z $DISPLAY ]]; then
        upload_action
    else
        upload_action | tr -d '\n' | xsel -b
    fi
}

# Record specific window
record () {
    ffmpeg -f x11grab -framerate 25 $(slop -f '-video_size %wx%h -i +%x,%y') $(mktemp -u -t 'XXXXXX' --suffix=.mp4)
}

# Combine 2 PDFs
pdfcombine () {
    gs -q -dNOPAUSE -sDEVICE=pdfwrite -sOUTPUTFILE=${1%.*}___${2%.*}.pdf -dBATCH $1 $2
}

# Calculate remaining worktime
worktime () {
    # Time Arithmetic

    TIME1=$(date "+%Y-%m-%d 07:00:00")
    TIME2=$(date "+%Y-%m-%d $1")

    # Convert the times to seconds from the Epoch
    SEC1=$(date -u -d "${TIME1}" +%s)
    SEC2=$(date -u -d "${TIME2}" +%s)

    # Use expr to do the math, let's say TIME1 was the start and TIME2 was the finish
    DIFFSEC=$(expr ${SEC2} - ${SEC1})
    DIFFSEC=${DIFFSEC#-}
    # And use date to convert the seconds back to something more meaningful

    TIMENOW=$(date "+%Y-%m-%d %H:%M:%S")
    TIMENOWTS=$(date -u -d "${TIMENOW}" +%s)
    ENDINGTS=$(expr ${TIMENOWTS} + ${DIFFSEC})
    ENDTIME=$(date -u -d "@${ENDINGTS}" "+%H:%M:%S")
    REMAINING=$(date -u -d "@${DIFFSEC}" "+%H:%M:%S")

    echo "Time Remaining :: ${REMAINING} | You're free to go at :: ${ENDTIME}"
}
