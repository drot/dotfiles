# Environment variables
export PAGER="less"
export LESS="-Ri"
export GREP_COLORS="mt=01;37;41"
export NAME="Davor Rotim"
export EMAIL="rotim.davor@nsoft.com"
export GROFF_NO_SGR=yes

# Check for Cargo binaries if available
[[ -d $HOME/.cargo/bin ]] && export PATH="$HOME/.cargo/bin:$PATH"

# Set PATH so it includes user directory
if [ -d "$HOME/.local/bin" ]; then
    export PATH="$HOME/.local/bin:$PATH"
fi

# Initialize Bash if available
[[ -r ~/.bashrc ]] && source ~/.bashrc
