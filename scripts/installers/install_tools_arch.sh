#!/usr/bin/env sh

set -x
set -e

env=/usr/bin/env
opwd-$PWD

cd $HOME

# Install the minimal toolset to bootstrap out process
sudo pacman -Syu
# Install the development requirements so we can use aur packages
sudo pacman -S --needed base-devel
# System basics for everything after this point
sudo pacman -S curl wget git zsh vim tmux

# Install OhMyZSH
mkdir -p $HOME/build/oh-my-zsh
cd $HOME/build/oh-my-zsh
curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -o install.sh
chmod +x install.sh
$env sh install.sh
cd $HOME

# Install package-query for yaourt
cd $HOME/build
git clone https://aur.archlinux.org/package-query.git
cd package-query
makepkg -sri

# Install yaourt
cd $HOME/build
git clone https://aur.archlinux.org/yaourt.git
cd yaourt
makepkg -sri

# Do a proper update including aur
cd $HOME
yaourt -Syua

# Install Other Essential Tools
yaourt -S lsb-release screenfetch

# Return to where we started
cd $opwd
