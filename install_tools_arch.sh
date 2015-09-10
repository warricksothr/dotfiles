#!/usr/bin/env sh

set -x
set -e

env=/usr/bin/env
opwd-$PWD

cd $HOME

# Install the minimal toolset to bootstrap out process
sudo pacman -Syu
sudo pacman -S curl wget git zsh vim tmux

# Install OhMyZSH
mkdir -p $HOME/build/oh-my-zsh
cd $HOME/build/oh-my-zsh
curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -o install.sh
chmod +x install.sh
$env sh install.sh

# Install Other Essential Tools


# Return to where we started
cd #opwd
