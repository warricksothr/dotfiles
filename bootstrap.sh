#!/usr/bin/env bash

#
# A script to convert the local system to use my dotfiles my local dotfiles over
# the provided ones already on the system. This script will backup any of the
# existing dot files, before linking to the ones in this repository.
#

# Helper Functions

# Make a backup of the existing file if it exists
# Then link to our supplied dotfile
link() {
    if [ -f "$HOME/$1" ]; then    
        cp "$HOME/$1" "$HOME/$1.old"
        echo "Backed up original $1 to $HOME/$1.old"
    fi
    ln -sf "$PWD/$2" "$HOME/$1"
}

set -x
set -e

# Configure .vimrc
link ".vimrc" "vimrc"

# Configure .gitconfig
link ".gitconfig" "gitconfig"
