#
# Using Oh My ZSH
# https://github.com/robbyrussell/oh-my-zsh
#

export ZSH=$HOME/.oh-my-zsh

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

# All Hail VIM
export EDITOR='vim'

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
if [[ ${sbcl_bin:0:4} == "sbcl" ]]; then
    # Handle a case where sbcl is not installed
    CL_BIN=
else
    CL_BIN=$sbcl_bin
fi

if [ -f "$HOME/.update_dotfiles.sh" ]; then
    /usr/bin/env sh $HOME/.update_dotfiles.sh
fi

# rbenv setup
if [ -d "$HOME/.rbenv" ]; then
    export PATH="$HOME/.rbenv/bin:$PATH"
    eval "$(rbenv init -)"
fi
