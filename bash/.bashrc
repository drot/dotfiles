# shellcheck shell=bash

# Only configure interactive shells
[[ $- != *i* ]] && return

# Source global definitions
[[ -r /etc/bash.bashrc ]] && source /etc/bash.bashrc

# Remap stop key for flow control
[[ -t 0 ]] && stty stop ^P

# Notify of completed background jobs immediately
set -o notify

# Prevent file overwrite on stdout redirection
set -o noclobber

# Shell behavior options
shopt -s cdspell # correct spelling errors in arguments supplied to cd
shopt -s dirspell # correct spelling errors during tab-completion
shopt -s extglob # turn on extended globbing
shopt -s no_empty_cmd_completion # disable tab-completion on an empty line

# History format and size
HISTSIZE=-1
HISTFILESIZE="${HISTSIZE}"
HISTCONTROL="ignoreboth:erasedups"
HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear"
HISTTIMEFORMAT="%F %T "

# History options
shopt -s histappend # append rather than overwrite history on shell exit
shopt -s histverify # allow history replacement editing
shopt -s cmdhist # save multi-line commands as one command

# Colored listings
if type -p dircolors >/dev/null; then
    eval "$(dircolors -b ~/.dircolors 2>/dev/null || dircolors -b)"
fi

# Load aliases
[[ -r ~/.bash_aliases ]] && source ~/.bash_aliases

# Load custom functions
[[ -r ~/.bash_functions ]] && source ~/.bash_functions

# Prompt colors
RED=""
GREEN=""
BLUE=""
RESET=""
if [[ -t 1 && -n $TERM ]] &&
    type -p tput >/dev/null &&
    tput setaf 1 >/dev/null 2>&1; then
    RED="\[$(tput setaf 1)\]"
    GREEN="\[$(tput setaf 2)\]"
    BLUE="\[$(tput setaf 4)\]"
    RESET="\[$(tput sgr0)\]"
fi

# Preserve the previous command's status and save new history
PROMPT_EXIT_STATUS=0
__prompt_command () {
    local exit_status=$?

    # shellcheck disable=SC2034 # expanded indirectly while rendering PS1
    PROMPT_EXIT_STATUS=$exit_status
    history -a
    return "$exit_status"
}

# Register our prompt hook without discarding existing hooks
_prompt_command_registered=""
for _prompt_command in "${PROMPT_COMMAND[@]}"; do
    [[ $_prompt_command == "__prompt_command" ]] &&
        _prompt_command_registered="yes"
done

if [[ -z $_prompt_command_registered ]]; then
    if [[ $(declare -p PROMPT_COMMAND 2>/dev/null) == "declare -a"* ]]; then
        PROMPT_COMMAND=("__prompt_command" "${PROMPT_COMMAND[@]}")
    elif [[ -n ${PROMPT_COMMAND[0]:-} ]]; then
        PROMPT_COMMAND=("__prompt_command" "${PROMPT_COMMAND[0]}")
    else
        PROMPT_COMMAND=("__prompt_command")
    fi
fi
unset _prompt_command _prompt_command_registered

# Trim deep directory paths
PROMPT_DIRTRIM="2"

# Git prompt support
[[ -r /usr/lib/git-core/git-sh-prompt ]] &&
    source /usr/lib/git-core/git-sh-prompt

# Git prompt format
GIT="\$(declare -F __git_ps1 &>/dev/null && __git_ps1 ' %s')"
GIT_PS1_SHOWDIRTYSTATE="yes"

# Prompt window title
TITLE="\[\e]2;\u@\h:\W\a\]"

# Make dynamic prompt based on exit command value
ERROR_CODE="\$(code=\${PROMPT_EXIT_STATUS##0}; echo \${code:+${GREEN}(${RED}\${code}${GREEN}) ${RESET}})"

# Check if we are on a SSH connection
SSH_CONN=""
if [[ -n ${SSH_CONNECTION:-} || -n ${SSH_CLIENT:-} || -n ${SSH_TTY:-} ]]; then
    SSH_CONN="${RED}@ "
fi

# Prompt format
case $TERM in
    foot|*256*)
        PS1="${TITLE}${ERROR_CODE}${SSH_CONN}${BLUE}\w${RED}${GIT}${GREEN} > ${RESET}"
        ;;
    eat-truecolor)
        PS1="${BLUE}\w${RED}${GIT}${GREEN} > ${RESET}"
        # Eat integration
        [[ -n $EAT_SHELL_INTEGRATION_DIR ]] && source "$EAT_SHELL_INTEGRATION_DIR/bash"
        ;;
    *)
        PS1="${SSH_CONN}${ERROR_CODE}${BLUE}\w${RED}${GIT}${GREEN} > ${RESET}"
        ;;
esac
