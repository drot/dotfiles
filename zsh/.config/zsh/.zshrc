# Only configure interactive shells
[[ -o interactive ]] || return

# Remap stop key for flow control
[[ -t 0 ]] && stty stop '^P'

# Shell behavior options
setopt AUTO_CD           # cd into a directory by typing its name
setopt NOTIFY            # report completed background jobs immediately
setopt NOCLOBBER         # prevent file overwrite on stdout redirection
setopt CORRECT           # offer spelling correction for commands
setopt EXTENDED_GLOB     # turn on extended globbing
setopt NO_BEEP           # disable beep

# History format and size
HISTFILE="$HOME/.zsh_history"
HISTSIZE=1000000
SAVEHIST=$HISTSIZE
HISTORY_IGNORE='(exit|ls|bg|fg|history|clear)'

# History options
setopt EXTENDED_HISTORY       # save timestamps
setopt SHARE_HISTORY          # synchronize history between open shells
setopt HIST_IGNORE_SPACE      # skip commands starting with a space
setopt HIST_IGNORE_ALL_DUPS   # erase older duplicates
setopt HIST_SAVE_NO_DUPS
setopt HIST_FIND_NO_DUPS
setopt HIST_REDUCE_BLANKS
setopt HIST_VERIFY            # allow history replacement editing

# Colored listings, using GNU coreutils when available
if (( $+commands[gdircolors] )); then
    eval "$(gdircolors -b ~/.dircolors 2>/dev/null || gdircolors -b)"
    alias ls="gls -h --group-directories-first --color=auto"
else
    export CLICOLOR=1
    alias ls="ls -Gh"
fi

# Load aliases and custom functions
[[ -r $ZDOTDIR/aliases.zsh ]] && source "$ZDOTDIR/aliases.zsh"
[[ -r $ZDOTDIR/functions.zsh ]] && source "$ZDOTDIR/functions.zsh"

# Completion search path
typeset -U fpath
for _completion_dir in \
    "$HOME/.local/share/zsh/site-functions" \
    /opt/homebrew/share/zsh/site-functions
do
    [[ -d $_completion_dir ]] && fpath=("$_completion_dir" $fpath)
done
unset _completion_dir

# Initialize completion
zmodload zsh/complist
autoload -Uz compinit bashcompinit
[[ -d $HOME/.cache/zsh ]] || mkdir -p "$HOME/.cache/zsh"
compinit -d "$HOME/.cache/zsh/zcompdump"
bashcompinit

# Include hidden files in completion without affecting globbing
_comp_options+=(globdots)

# Completion options
setopt AUTO_MENU          # cycle through matches on repeated tab
setopt NO_LIST_AMBIGUOUS  # show all matches on the first tab
setopt LIST_TYPES         # append file type when listing completions
setopt COMPLETE_IN_WORD   # skip already completed text after the cursor
setopt AUTO_PARAM_SLASH   # add trailing slashes to directories and symlinks
zstyle ':completion:*' completer _complete _approximate
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*' menu select
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# Enable Terraform completion if available
(( $+commands[terraform] )) && complete -C "${commands[terraform]}" terraform

# Disable tab-completion on an empty line
_complete_unless_empty () {
    [[ -z ${BUFFER//[[:space:]]/} ]] && return
    zle expand-or-complete
}
zle -N _complete_unless_empty

# Emacs key bindings
bindkey -e

# Use Emacs-style history search keys
bindkey '^[p' history-beginning-search-backward
bindkey '^[n' history-beginning-search-forward

# Use completion menu and allow cycling
bindkey '^I' _complete_unless_empty
bindkey '^[[Z' reverse-menu-complete
bindkey -M menuselect '^[[Z' reverse-menu-complete

# Edit command line in $EDITOR
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey '^X^E' edit-command-line

# Git prompt support
autoload -Uz add-zsh-hook vcs_info
setopt PROMPT_SUBST
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' unstagedstr '*'
zstyle ':vcs_info:git:*' stagedstr '+'
zstyle ':vcs_info:git:*' formats ' %b%u%c'
zstyle ':vcs_info:git:*' actionformats ' %b|%a%u%c'

_prompt_precmd () {
    vcs_info

    case $TERM in
        eat-truecolor) ;;
        *) print -Pn '\e]2;%n@%m:%1~\a' ;;
    esac

    # Prompt jumping
    case $TERM in
        tmux-256color) print -n '\e]133;A\e\\' ;;
    esac
}
add-zsh-hook precmd _prompt_precmd

# Prompt components
PROMPT_ERROR='%(?..%F{green}(%F{red}%?%F{green}%) %f)'
PROMPT_SSH=''
if [[ -n ${SSH_CONNECTION:-}${SSH_CLIENT:-}${SSH_TTY:-} ]]; then
    PROMPT_SSH='%F{red}@ %f'
fi
PROMPT_DIR='%F{blue}%(4~|%-1~/.../%2~|%~)'
PROMPT_GIT='%F{red}${vcs_info_msg_0_}'

# Prompt format
case $TERM in
    eat-truecolor)
        PROMPT="${PROMPT_DIR}${PROMPT_GIT}%F{green} > %f"
        # Eat integration
        [[ -n ${EAT_SHELL_INTEGRATION_DIR:-} ]] &&
            source "$EAT_SHELL_INTEGRATION_DIR/zsh"
        ;;
    *)
        PROMPT="${PROMPT_ERROR}${PROMPT_SSH}${PROMPT_DIR}${PROMPT_GIT}%F{green} > %f"
        ;;
esac
