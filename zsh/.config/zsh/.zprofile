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

# Prepend user directories to PATH if they exist
typeset -U path PATH
for _user_bin in \
    "${JAVA_HOME:+$JAVA_HOME/bin}" \
    "$HOME/.cargo/bin" \
    "$HOME/go/bin" \
    "$HOME/.opencode/bin" \
    "$HOME/.kafka-tools/bin" \
    "$HOME/.local/bin"
do
    [[ -d $_user_bin ]] && path=("$_user_bin" $path)
done
unset _user_bin
