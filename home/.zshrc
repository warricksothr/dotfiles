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
alias time="/usr/bin/time --format='Command:%C \nElapsed Time: %E\nUser Time: %U\nSystem Time: %S\nCPU: %P\nMax Memory: %MKb\nAverage Memory: %KKb\nAverage Unshared Memory: %DKb\nNumber of Swaps: %W\nNumber of Waits: %w\nExit Status: %x'"
alias rename="noglob rename"

if [ -f "$HOME/.update_dotfiles.sh" ]; then
    /usr/bin/env sh $HOME/.update_dotfiles.sh
fi
