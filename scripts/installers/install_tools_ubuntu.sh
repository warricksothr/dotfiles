#!/usr/bin/env sh

set -x
set -e

env=/usr/bin/env
opwd=$PWD
package_manager="sudo apt-get"
pm_update="$package_manager update"
pm_install="$package_manager install"

cd $HOME

# Install the minimal toolset to bootstrap out process
$pm_update
# System basics for everything after this point
$pm_install curl wget git zsh vim tmux mosh python-software-properties software-properties-common

# Install OhMyZSH
mkdir -p $HOME/build/oh-my-zsh
cd $HOME/build/oh-my-zsh
curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -o install.sh
chmod +x install.sh
$env sh install.sh
cd $HOME

# Add the screenfetch repository
sudo add-apt-repository -y 'ppa:djcj/screenfetch'

# Install Other Essential Tools
$pm_install lsb-release screenfetch

# Return to where we started
cd $opwd
