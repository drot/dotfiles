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

# Paste a file or URL to 0x0.st and copy the resulting URL with pbcopy.
0x0 () {
    local -a form_args
    local response
    local url="https://0x0.st"

    if (( $# != 2 )); then
        print -u2 "Usage: 0x0 {-f FILE|-u URL|-s URL}"
        return 2
    fi

    case $1 in
        -f)
            if [[ ! -f $2 || ! -r $2 ]]; then
                print -u2 -r -- "0x0: file is not readable: $2"
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
            print -u2 "Usage: 0x0 {-f FILE|-u URL|-s URL}"
            return 2
            ;;
    esac

    response=$(command curl --fail --show-error --silent \
        --connect-timeout 10 --max-time 300 \
        "${form_args[@]}" "$url") || return

    if [[ -z $response ]]; then
        print -u2 "0x0: server returned an empty response"
        return 1
    fi

    print -r -- "$response"
    if (( $+commands[pbcopy] )); then
        print -rn -- "$response" | pbcopy ||
            print -u2 "0x0: could not copy response with pbcopy"
    fi
}

# Calculate when a seven-hour workday will be complete.
worktime () {
    if (( $# != 1 )); then
        print -u2 "Usage: worktime HH:MM[:SS]"
        return 2
    fi

    local hours minutes seconds
    local required_seconds worked_seconds remaining_seconds
    local now ending_ts end_time remaining

    if [[ $1 =~ '^([0-9]{1,2}):([0-5][0-9]):([0-5][0-9])$' ]]; then
        hours=$match[1]
        minutes=$match[2]
        seconds=$match[3]
    elif [[ $1 =~ '^([0-9]{1,2}):([0-5][0-9])$' ]]; then
        hours=$match[1]
        minutes=$match[2]
        seconds=0
    else
        print -u2 -r -- "worktime: invalid time: $1"
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
        $((remaining_seconds / 3600)) \
        $(((remaining_seconds % 3600) / 60)) \
        $((remaining_seconds % 60))

    print -r -- "Time Remaining :: $remaining | You're free to go at :: $end_time"
}

# Show matching process IDs and full command lines using macOS pgrep.
pids () {
    if (( $# == 0 )); then
        print -u2 "Usage: pids PATTERN"
        return 2
    fi
    command pgrep -fl -- "$@"
}
