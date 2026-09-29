# PATH is set here rather than in .zshenv because /etc/zprofile runs
# path_helper, which reorders any PATH entries set before it.

# Initialize Homebrew on Apple Silicon before configuring other tools
if [[ -x /opt/homebrew/bin/brew ]]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# Use Homebrew's keg-only OpenJDK (needed by the Kafka CLI tools)
_jdk="${HOMEBREW_PREFIX:-/opt/homebrew}/opt/openjdk@21/libexec/openjdk.jdk/Contents/Home"
[[ -d $_jdk ]] && export JAVA_HOME="$_jdk"
unset _jdk

# Prepend user directories to PATH; earlier entries take precedence,
# missing directories are skipped by the (N-/) glob qualifier
typeset -U path PATH
_user_bins=(
    $HOME/.local/bin
    $HOME/.kafka-tools/bin
    ${JAVA_HOME:+$JAVA_HOME/bin}
    $HOME/.opencode/bin
    $HOME/go/bin
    $HOME/.cargo/bin
)
path=(${^_user_bins}(N-/) $path)
unset _user_bins
