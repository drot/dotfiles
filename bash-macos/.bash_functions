#!/opt/homebrew/bin/bash

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

    if type -P pbcopy >/dev/null; then
        printf '%s' "$response" | pbcopy ||
            echo "0x0: could not copy response with pbcopy" >&2
    fi

    return 0
}

# macOS provides interactive screen recording with Shift-Command-5.
record () {
    if (( $# > 1 )); then
        echo "Usage: record [OUTPUT.mp4]" >&2
        return 2
    fi

    echo "record: use Shift-Command-5 for native macOS screen recording" >&2
    return 1
}

# Combine 2 PDFs
pdfcombine () {
    local force=""

    if [[ ${1:-} == "-f" ]]; then
        force="yes"
        shift
    fi

    if (( $# != 2 )); then
        echo "Usage: pdfcombine [-f] FIRST.pdf SECOND.pdf" >&2
        return 2
    fi

    local first=$1
    local second=$2

    [[ $first == -* ]] && first="./$first"
    [[ $second == -* ]] && second="./$second"

    local second_name=${second##*/}
    local output="${first%.*}___${second_name%.*}.pdf"

    if ! type -P gs >/dev/null; then
        echo "pdfcombine: Ghostscript is required" >&2
        return 127
    fi

    if [[ ! -f $first || ! -r $first ]]; then
        printf 'pdfcombine: input is not readable: %s\n' "$first" >&2
        return 1
    fi

    if [[ ! -f $second || ! -r $second ]]; then
        printf 'pdfcombine: input is not readable: %s\n' "$second" >&2
        return 1
    fi

    if [[ -e $output && -z $force ]]; then
        printf 'pdfcombine: output already exists: %s (use -f to replace it)\n' \
            "$output" >&2
        return 1
    fi

    gs -q -dNOPAUSE -sDEVICE=pdfwrite -sOUTPUTFILE="$output" \
        -dBATCH "$first" "$second"
}

# Calculate remaining worktime
worktime () {
    if (( $# != 1 )); then
        echo "Usage: worktime HH:MM[:SS]" >&2
        return 2
    fi

    local hours minutes seconds
    local required_seconds worked_seconds remaining_seconds
    local now ending_ts end_time remaining

    if [[ $1 =~ ^([0-9]{1,2}):([0-5][0-9]):([0-5][0-9])$ ]]; then
        hours=${BASH_REMATCH[1]}
        minutes=${BASH_REMATCH[2]}
        seconds=${BASH_REMATCH[3]}
    elif [[ $1 =~ ^([0-9]{1,2}):([0-5][0-9])$ ]]; then
        hours=${BASH_REMATCH[1]}
        minutes=${BASH_REMATCH[2]}
        seconds=0
    else
        printf 'worktime: invalid time: %s\n' "$1" >&2
        return 2
    fi

    required_seconds=$((7 * 60 * 60))
    worked_seconds=$((10#$hours * 60 * 60 + 10#$minutes * 60 + 10#$seconds))
    remaining_seconds=$((required_seconds - worked_seconds))
    (( remaining_seconds < 0 )) && remaining_seconds=0

    now=$(date +%s) || return
    ending_ts=$((now + remaining_seconds))
    end_time=$(date -r "$ending_ts" "+%H:%M:%S") || return
    printf -v remaining '%02d:%02d:%02d' \
        "$((remaining_seconds / 3600))" \
        "$(((remaining_seconds % 3600) / 60))" \
        "$((remaining_seconds % 60))"

    echo "Time Remaining :: $remaining | You're free to go at :: $end_time"
}
