#
# Using Oh My ZSH
# https://github.com/robbyrussell/oh-my-zsh
#

export ZSH=$HOME/.oh-my-zsh

# Detect if we're running in cygwin
local cygwin="$(env | grep cygdrive &>/dev/null && echo $?)"
if [ -n "${cygwin}" ] && [[ "${cygwin}" == "0" ]]; then
    export CSYSTEM="CSYS"
fi

# Best theme ever!
ZSH_THEME="ys"

# System variables for tmux configuration
export ZSH_TMUX_AUTOSTART=false
export ZSH_TMUX_AUTOSTART_ONCE=true

# Used zsh plugins
plugins=(archlinux systemd common-aliases history screen tmux wd git git-extras mercurial)

# Let's update and do other goodness
source $ZSH/oh-my-zsh.sh

# Make sure we're in en_us UTF8
export LANG=en_US.UTF-8

# All Hail EMACS
export EDITOR='emacsclient -a vim'

# Make sure ~/bin is on the path
export PATH="$HOME/bin:$PATH"

# Add something to something else
# $1 is the original value being appended to
# $2 is the seperator
# $3 is what we should add as long as it has some value
function append_if_exists() {
    result="$1"
    if [ ! -z "$1" ] && [ ! -z "$2" ] && [ ! -z "$3" ]; then
        result="${result}${2}${3}"
    fi
    echo $result
}

# Alias to emacsclient
# fall back on vim if emacs isn't started
alias ec="emacsclient -a vim"

# Don't use the embedded shell time. Use GNU time.
alias time="/usr/bin/time"
# Alias for GNU Time with pretty output.
alias ti="/usr/bin/time --format='Command:%C \nElapsed Time: %E\nUser Time: %U\nSystem Time: %S\nCPU: %P\nMax Memory: %MKb\nAverage Memory: %KKb\nAverage Unshared Memory: %DKb\nNumber of Swaps: %W\nNumber of Waits: %w\nExit Status: %x'"

if [ -d "$HOME/.linuxbrew" ]; then
    export PATH="$HOME/.linuxbrew/bin:$PATH"
    export MANPATH="$HOME/.linuxbrew/share/man:$MANPATH"
    export INFOPATH="$HOME/.linuxbrew/share/info:$INFOPATH"
fi

# Go Setup
export GOPATH="$HOME/go"
export PATH="$PATH:$HOME/go/bin"

# CL Setup
# if SBCL is installed point to that path
sbcl_bin="$(which sbcl 2> /dev/null | head -n 1)"
if [[ ${sbcl_bin:0:4} != "sbcl" ]]; then
    # Handle a case where sbcl is not installed
    CL_BIN=$sbcl_bin
fi

if [ -f "$HOME/.update_dotfiles.sh" ]; then
    /usr/bin/env sh $HOME/.update_dotfiles.sh
fi

# rbenv setup
# # doesn't work under msys
if [ -d "$HOME/.rbenv" ] && [ -z "$MSYSTEM" ]; then
    export PATH="$HOME/.rbenv/bin:$PATH"
    eval "$(rbenv init -)"
fi

# Rust Setup
if [ -d "$HOME/.cargo" ]; then
    export PATH="$HOME/.cargo/bin:$PATH"
fi

# A function that does the automatic path translation for a windows emacs
cygwin_emacsclient(){
    # Loop over all the input variables and convert their paths     
    declare -a args=() 
    for arg in $@
    do
        win_path="$(cygpath -p -w $arg)"
        args=("${args[@]}" "${win_path}")
    done
    # Execute emacsclient with the correct paths
    emacsclient -a vim $args[@]
}

# Cygwin Specific Configuration
if [ -n "${CSYSTEM}" ] && [[ "${CSYSTEM}" == "CSYS" ]]; then
    # Configure emacsclient to get the correct paths
    emacs_path="$(which emacs)"
    if [[ $emacs_path == /cygdrive* ]]; then
        alias ec=cygwin_emacsclient
    fi

fi

# MSYS Specific Configuration
if [ -n "$MSYSTEM" ] && [[ "$MSYSTEM" == "MSYS" ]]; then
    # Make sure msys has our CL on the path
    export PATH=$(append_if_exists $PATH ":" $(dirname "$CL_BIN"))
fi
