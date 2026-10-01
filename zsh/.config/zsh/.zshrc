# Interactive shells only
[[ -o interactive ]] || return

#
# Shell options
#

# Directories
setopt AUTO_CD # type a dir name to cd into it
setopt AUTO_PUSHD # cd pushes onto dir stack (cd -<TAB>, cd -2)
setopt PUSHD_IGNORE_DUPS # no duplicates in dir stack
setopt PUSHD_SILENT # don't print dir stack on pushd/popd

# Input and output
setopt NOCLOBBER # > won't overwrite files (>| forces)
setopt EXTENDED_GLOB # extended glob patterns (^, ~, #)
setopt NO_BEEP # no beep
setopt NO_FLOW_CONTROL # free ^S/^Q from terminal freeze

# Correction
setopt CORRECT # suggest fixes for mistyped commands
SPROMPT='zsh: correct %F{red}%R%f to %F{green}%r%f [nyae]? ' # n=no y=yes a=abort e=edit

REPORTTIME=10 # show timing for commands over 10s

#
# History
#

HISTFILE="$HOME/.zsh_history"
HISTSIZE=1000000
SAVEHIST=$HISTSIZE
HISTORY_IGNORE='(exit|ls|bg|fg|history|clear)' # never saved

setopt EXTENDED_HISTORY # save timestamps
setopt SHARE_HISTORY # share history between shells
setopt HIST_IGNORE_SPACE # skip commands starting with a space
setopt HIST_IGNORE_ALL_DUPS # drop older duplicates
setopt HIST_REDUCE_BLANKS # trim extra whitespace
setopt HIST_VERIFY # show !! expansion before running

#
# Commands, aliases and functions
#

# Colored ls (GNU ls if installed)
if (( $+commands[gdircolors] )); then
    eval "$(gdircolors -b ~/.dircolors 2>/dev/null || gdircolors -b)"
    alias ls="gls -h --group-directories-first --color=auto"
else
    export CLICOLOR=1
    alias ls="ls -Gh"
fi

[[ -r $ZDOTDIR/aliases.zsh ]] && source "$ZDOTDIR/aliases.zsh"
[[ -r $ZDOTDIR/functions.zsh ]] && source "$ZDOTDIR/functions.zsh"

# Bulk rename: zmv '(*).jpeg' '$1.jpg' (-n = dry run)
autoload -Uz zmv

# M-h: help for the command on the line (zsh builtins, git, man)
(( $+aliases[run-help] )) && unalias run-help
autoload -Uz run-help run-help-git
[[ -d /usr/share/zsh/$ZSH_VERSION/help ]] &&
    HELPDIR=/usr/share/zsh/$ZSH_VERSION/help

#
# Completion
#

# Extra completion dir (Homebrew's comes from brew shellenv)
typeset -U fpath
[[ -d $HOME/.local/share/zsh/site-functions ]] &&
    fpath=("$HOME/.local/share/zsh/site-functions" $fpath)

zmodload zsh/complist
autoload -Uz compinit
[[ -d $HOME/.cache/zsh ]] || mkdir -p "$HOME/.cache/zsh"
compinit -d "$HOME/.cache/zsh/zcompdump"

_comp_options+=(globdots) # complete hidden files

setopt NO_LIST_AMBIGUOUS # list matches on first tab
setopt COMPLETE_IN_WORD # complete from the cursor position

zstyle ':completion:*' completer _complete _approximate # then fix typos
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' # case-insensitive
zstyle ':completion:*' menu select # arrow-key menu
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS} # colors like ls
zstyle ':completion:*' use-cache on # cache slow completers
zstyle ':completion:*' cache-path "$HOME/.cache/zsh/compcache"
zstyle ':completion:*' group-name '' # group by type
zstyle ':completion:*:descriptions' format '%F{yellow}-- %d --%f' # group headers
zstyle ':completion:*:corrections' format '%F{yellow}-- %d (errors: %e) --%f' # typo-fix header

#
# Line editor and key bindings
#

bindkey -e # emacs keymap (must precede other bindkeys)

# Tab: complete (not on empty line); Shift-Tab: previous match
_complete_unless_empty () {
    [[ -z ${BUFFER//[[:space:]]/} ]] && return
    zle expand-or-complete
}
zle -N _complete_unless_empty
bindkey '^I' _complete_unless_empty
bindkey '^[[Z' reverse-menu-complete
bindkey -M menuselect '^[[Z' reverse-menu-complete

# M-b / M-f / M-DEL: bash-style words (letters and digits only)
autoload -Uz select-word-style
select-word-style bash

# ^W: delete back to previous space
autoload -Uz backward-kill-word-match
zle -N unix-word-rubout backward-kill-word-match
zstyle ':zle:unix-word-rubout' word-style whitespace
bindkey '^W' unix-word-rubout

# M-p / M-n: history prefix search, cursor stays
bindkey '^[p' history-beginning-search-backward
bindkey '^[n' history-beginning-search-forward

# ^P / ^N / Up / Down: history prefix search, cursor to end
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey '^P' up-line-or-beginning-search
bindkey '^N' down-line-or-beginning-search
bindkey '^[[A' up-line-or-beginning-search
bindkey '^[OA' up-line-or-beginning-search
bindkey '^[[B' down-line-or-beginning-search
bindkey '^[OB' down-line-or-beginning-search

# M-, : after M-., cycle through earlier words of that command
autoload -Uz copy-earlier-word
zle -N copy-earlier-word
bindkey '^[,' copy-earlier-word

bindkey ' ' magic-space # space expands !!, !$ in place

# Auto-quote URLs when typing or pasting
autoload -Uz bracketed-paste-magic url-quote-magic
zle -N bracketed-paste bracketed-paste-magic
zle -N self-insert url-quote-magic

# ^X^E: edit command line in $VISUAL
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey '^X^E' edit-command-line

#
# Prompt
#

# Git info: branch, * unstaged, + staged, ? untracked
autoload -Uz add-zsh-hook vcs_info
setopt PROMPT_SUBST
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' unstagedstr '*'
zstyle ':vcs_info:git:*' stagedstr '+'
zstyle ':vcs_info:git:*' formats ' %b%u%c'
zstyle ':vcs_info:git:*' actionformats ' %b|%a%u%c'
zstyle ':vcs_info:git*+set-message:*' hooks git-untracked

+vi-git-untracked () {
    local REPLY
    command git ls-files --others --exclude-standard \
        --directory --no-empty-directory 2>/dev/null | read -r &&
        hook_com[unstaged]+='?'
}

# Prompt marks (OSC 133) for iTerm2 and tmux: jump between prompts, select output
[[ $TERM_PROGRAM == iTerm.app || $TERM == tmux-256color ]] && _prompt_marks=1

# Before each prompt
_prompt_precmd () {
    local ret=$? # must be first, before anything changes $?

    # Mark end of previous command's output with its exit status
    if (( _prompt_marks && _prompt_cmd_ran )); then
        print -n "\e]133;D;$ret\a"
    fi
    _prompt_cmd_ran=0

    vcs_info

    # Window and tab title: user@host:dir
    case $TERM in
        eat-truecolor) ;;
        *) print -Pn '\e]0;%n@%m:%1~\a' ;;
    esac

    # Tell terminal the cwd (OSC 7) so new tabs open here
    local LC_ALL=C cwd= ch
    for ch in ${(s::)PWD}; do
        if [[ $ch == [[:alnum:]/._~-] ]]; then
            cwd+=$ch
        else
            cwd+=%${(l:2::0:)$(( [##16] #ch ))}
        fi
    done
    print -n "\e]7;file://${HOST}${cwd}\a"
}
add-zsh-hook precmd _prompt_precmd

# Before each command runs: mark start of output
_prompt_preexec () {
    _prompt_cmd_ran=1
    (( _prompt_marks )) && print -n '\e]133;C\a'
}
add-zsh-hook preexec _prompt_preexec

PROMPT_ERROR='%(?..%F{green}(%F{red}%?%F{green}%) %f)' # (code) if last command failed
PROMPT_SSH='' # red @ over SSH
if [[ -n ${SSH_CONNECTION:-}${SSH_CLIENT:-}${SSH_TTY:-} ]]; then
    PROMPT_SSH='%F{red}@ %f'
fi
PROMPT_DIR='%F{blue}%(4~|%-1~/.../%2~|%~)' # cwd, shortened when deep
PROMPT_GIT='%F{red}${vcs_info_msg_0_}'
# Prompt start/end marks; start must be in PROMPT, zsh clears the line after precmd
PROMPT_MARK_START=''
PROMPT_MARK_END=''
if (( _prompt_marks )); then
    PROMPT_MARK_START=$'%{\e]133;A\a%}'
    PROMPT_MARK_END=$'%{\e]133;B\a%}'
fi

case $TERM in
    eat-truecolor)
        PROMPT="${PROMPT_DIR}${PROMPT_GIT}%F{green} > %f"
        # Emacs Eat terminal integration
        [[ -n ${EAT_SHELL_INTEGRATION_DIR:-} ]] &&
            source "$EAT_SHELL_INTEGRATION_DIR/zsh"
        ;;
    *)
        PROMPT="${PROMPT_MARK_START}${PROMPT_ERROR}${PROMPT_SSH}${PROMPT_DIR}${PROMPT_GIT}%F{green} > %f${PROMPT_MARK_END}"
        ;;
esac
