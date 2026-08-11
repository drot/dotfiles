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
    local -a form_args
    local response
    local url="https://0x0.st"

    if (( $# != 2 )); then
        echo "Usage: 0x0 {-f FILE|-u URL|-s URL}" >&2
        return 2
    fi

    if ! type -P curl >/dev/null; then
        echo "0x0: curl is required" >&2
        return 127
    fi

    case $1 in
        -f)
            if [[ ! -f $2 || ! -r $2 ]]; then
                printf '0x0: file is not readable: %s\n' "$2" >&2
                return 1
            fi
            form_args=(-F "file=@$2")
            ;;
        -u)
            form_args=(--form-string "url=$2")
            ;;
        -s)
            form_args=(--form-string "shorten=$2")
            ;;
        *)
            echo "Usage: 0x0 {-f FILE|-u URL|-s URL}" >&2
            return 2
    esac

    response=$(command curl --fail --show-error --silent \
        --connect-timeout 10 --max-time 300 \
        "${form_args[@]}" "$url") || return

    if [[ -z $response ]]; then
        echo "0x0: server returned an empty response" >&2
        return 1
    fi

    printf '%s\n' "$response"

    # Copy the URL when a supported graphical clipboard is available
    if [[ -n ${WAYLAND_DISPLAY:-} ]] && type -P wl-copy >/dev/null; then
        printf '%s' "$response" | wl-copy ||
            echo "0x0: could not copy response with wl-copy" >&2
    elif [[ -n ${DISPLAY:-} ]] && type -P xsel >/dev/null; then
        printf '%s' "$response" | xsel --clipboard --input ||
            echo "0x0: could not copy response with xsel" >&2
    fi

    return 0
}

# Record specific window
record () {
    local -a capture_args
    local output_dir output

    read -r -a capture_args < <(slop -f '-video_size %wx%h -i +%x,%y') ||
        return
    output_dir=$(mktemp -d -t 'record.XXXXXX') || return
    output="$output_dir/recording.mp4"

    ffmpeg -f x11grab -framerate 25 "${capture_args[@]}" "$output" &&
        printf 'Saved recording to %s\n' "$output"
}

# Combine 2 PDFs
pdfcombine () {
    if (( $# != 2 )); then
        echo "Usage: pdfcombine FIRST.pdf SECOND.pdf" >&2
        return 2
    fi

    local second_name=${2##*/}
    local output="${1%.*}___${second_name%.*}.pdf"

    gs -q -dNOPAUSE -sDEVICE=pdfwrite -sOUTPUTFILE="$output" \
        -dBATCH "$1" "$2"
}

# Calculate remaining worktime
worktime () {
    if (( $# != 1 )); then
        echo "Usage: worktime HH:MM[:SS]" >&2
        return 2
    fi

    local time1 time2 sec1 sec2 diffsec
    local time_now time_now_ts ending_ts end_time remaining

    # Time Arithmetic

    time1=$(date "+%Y-%m-%d 07:00:00")
    time2=$(date "+%Y-%m-%d $1")

    # Convert the times to seconds from the Epoch
    sec1=$(date -u -d "$time1" +%s) || return
    sec2=$(date -u -d "$time2" +%s) || return

    # Calculate the absolute difference between start and finish
    diffsec=$((sec2 - sec1))
    diffsec=${diffsec#-}
    # And use date to convert the seconds back to something more meaningful

    time_now=$(date "+%Y-%m-%d %H:%M:%S")
    time_now_ts=$(date -u -d "$time_now" +%s) || return
    ending_ts=$((time_now_ts + diffsec))
    end_time=$(date -u -d "@$ending_ts" "+%H:%M:%S") || return
    remaining=$(date -u -d "@$diffsec" "+%H:%M:%S") || return

    echo "Time Remaining :: $remaining | You're free to go at :: $end_time"
}
