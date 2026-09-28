# Only configure interactive shells
[[ -o interactive ]] || return

# Disable flow control so ^S and ^Q reach the line editor
# (^S = forward history search instead of freezing the terminal)
setopt NO_FLOW_CONTROL

# Shell behavior options
setopt AUTO_CD           # cd into a directory by typing its name
setopt AUTO_PUSHD        # every cd pushes onto the dir stack; `cd -<TAB>` lists it, `cd -2` jumps
setopt PUSHD_IGNORE_DUPS # keep the directory stack free of duplicates
setopt PUSHD_SILENT      # do not print the directory stack after pushd/popd
setopt NOCLOBBER         # prevent file overwrite on stdout redirection (use >| to force)
setopt CORRECT           # offer spelling correction for commands
# Correction prompt: n = run as typed, y = run fix, a = abort, e = edit line
SPROMPT='zsh: correct %F{red}%R%f to %F{green}%r%f [nyae]? '
setopt EXTENDED_GLOB     # turn on extended globbing
setopt NO_BEEP           # disable beep

# Print time and CPU usage after commands running longer than 10 seconds
REPORTTIME=10

# Batch rename with patterns: zmv '(*).jpeg' '$1.jpg' (add -n for a dry run)
autoload -Uz zmv

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
setopt HIST_REDUCE_BLANKS     # strip superfluous whitespace before saving
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

# Completion search path (Homebrew's is added by `brew shellenv` in .zprofile)
typeset -U fpath
[[ -d $HOME/.local/share/zsh/site-functions ]] &&
    fpath=("$HOME/.local/share/zsh/site-functions" $fpath)

# Initialize completion
zmodload zsh/complist
autoload -Uz compinit
[[ -d $HOME/.cache/zsh ]] || mkdir -p "$HOME/.cache/zsh"
compinit -d "$HOME/.cache/zsh/zcompdump"

# Include hidden files in completion without affecting globbing
_comp_options+=(globdots)

# Completion options
setopt NO_LIST_AMBIGUOUS  # show all matches on the first tab
setopt COMPLETE_IN_WORD   # skip already completed text after the cursor
# Try normal completion first, then fuzzy matches that fix typos
zstyle ':completion:*' completer _complete _approximate
# Case-insensitive matching
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
# Navigable completion menu (arrow keys to move)
zstyle ':completion:*' menu select
# Color completion listings like ls
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
# Cache results of slow completers (brew, docker, kubectl, ...)
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "$HOME/.cache/zsh/compcache"
# Group matches by type under yellow headers (-- file --, -- command --, ...)
zstyle ':completion:*' group-name ''
zstyle ':completion:*:descriptions' format '%F{yellow}-- %d --%f'
# Header for typo-corrected matches from _approximate
zstyle ':completion:*:corrections' format '%F{yellow}-- %d (errors: %e) --%f'
# Disable tab-completion on an empty line
_complete_unless_empty () {
    [[ -z ${BUFFER//[[:space:]]/} ]] && return
    zle expand-or-complete
}
zle -N _complete_unless_empty

# Emacs key bindings
bindkey -e

# Words are only letters and digits, like Emacs
# (M-DEL, M-b, M-f stop at every -, ., /, _ and so on)
WORDCHARS=''

# M-p / M-n: search history for lines starting with the text before the cursor
bindkey '^[p' history-beginning-search-backward
bindkey '^[n' history-beginning-search-forward

# ^P / ^N and Up / Down: same prefix search, but the cursor moves to the end
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey '^P' up-line-or-beginning-search
bindkey '^N' down-line-or-beginning-search
bindkey '^[[A' up-line-or-beginning-search
bindkey '^[OA' up-line-or-beginning-search
bindkey '^[[B' down-line-or-beginning-search
bindkey '^[OB' down-line-or-beginning-search

# Quote URLs automatically when typed or pasted, so ? and & don't glob
autoload -Uz bracketed-paste-magic url-quote-magic
zle -N bracketed-paste bracketed-paste-magic
zle -N self-insert url-quote-magic

# M-. inserts the last word of the previous command; then
# M-, steps back through the earlier words of that command
autoload -Uz copy-earlier-word
zle -N copy-earlier-word
bindkey '^[,' copy-earlier-word

# Space expands history references (!!, !$, !*) in place before running
bindkey ' ' magic-space

# M-h: show help for the command on the line (zsh docs for builtins,
# git help for git subcommands, man pages otherwise)
(( $+aliases[run-help] )) && unalias run-help
autoload -Uz run-help run-help-git
[[ -d /usr/share/zsh/$ZSH_VERSION/help ]] &&
    HELPDIR=/usr/share/zsh/$ZSH_VERSION/help

# Tab completes (not on an empty line), Shift-Tab cycles backwards
bindkey '^I' _complete_unless_empty
bindkey '^[[Z' reverse-menu-complete
bindkey -M menuselect '^[[Z' reverse-menu-complete

# ^X^E: edit the current command line in $VISUAL / $EDITOR
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey '^X^E' edit-command-line

# Git prompt: branch name followed by * (unstaged), + (staged), ? (untracked)
autoload -Uz add-zsh-hook vcs_info
setopt PROMPT_SUBST
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' unstagedstr '*'
zstyle ':vcs_info:git:*' stagedstr '+'
zstyle ':vcs_info:git:*' formats ' %b%u%c'
zstyle ':vcs_info:git:*' actionformats ' %b|%a%u%c'
zstyle ':vcs_info:git*+set-message:*' hooks git-untracked

# Mark untracked files in the git prompt
+vi-git-untracked () {
    local REPLY
    command git ls-files --others --exclude-standard \
        --directory --no-empty-directory 2>/dev/null | read -r &&
        hook_com[unstaged]+='?'
}

# Runs before each prompt
_prompt_precmd () {
    vcs_info

    # Window title: user@host:dir
    case $TERM in
        eat-truecolor) ;;
        *) print -Pn '\e]2;%n@%m:%1~\a' ;;
    esac

    # Report working directory to the terminal (OSC 7), so new tabs
    # and splits can open in the same directory
    local LC_ALL=C cwd= ch
    for ch in ${(s::)PWD}; do
        if [[ $ch == [[:alnum:]/._~-] ]]; then
            cwd+=$ch
        else
            cwd+=%${(l:2::0:)$(( [##16] #ch ))}
        fi
    done
    print -n "\e]7;file://${HOST}${cwd}\a"

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
