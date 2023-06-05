# Check for interactive
[[ $- != *i* ]] && return

# Remap stop key for flow control
stty stop ^P

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
shopt -s histverify # allow history replacement editing
shopt -s cmdhist # save multi-line commands as one command

# Colored listings
if [[ -r ~/.dircolors ]] && type -p dircolors >/dev/null; then
    eval $(dircolors ~/.dircolors)
fi

# Load aliases
[[ -r ~/.bash_aliases ]] && source ~/.bash_aliases

# Load custom functions
[[ -r ~/.bash_functions ]] && source ~/.bash_functions

# Prompt colors
RED="\[$(tput setaf 1)\]"
GREEN="\[$(tput setaf 2)\]"
BLUE="\[$(tput setaf 4)\]"
RESET="\[$(tput sgr0)\]"

# Save history after each command execution
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"

# Trim deep directory paths
PROMPT_DIRTRIM="2"

# Git prompt support
[[ -r /usr/share/git/git-prompt.sh ]] && source /usr/share/git/git-prompt.sh

# Git prompt format
GIT="\$(declare -F __git_ps1 &>/dev/null && __git_ps1 ' %s')"
GIT_PS1_SHOWDIRTYSTATE="yes"

# Prompt window title
TITLE="\[\e]2;\u@\h:\W\a\]"

# Make dynamic prompt based on exit command value
ERROR_CODE="\$(code=\${?##0}; echo \${code:+${GREEN}(${RED}\${code}${GREEN}) ${RESET}})"

# Check if we are on a SSH connection
[[ -n $SSH_CLIENT ]] && SSH_CONN="${RED}@ "

# Prompt format
case $TERM in
    alacritty|*256*)
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
