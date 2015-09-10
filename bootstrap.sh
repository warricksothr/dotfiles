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
    # Only make a backup if an existing file is there and is not a link
    if [ -f "$1/$2" ] && [ ! -L "$1/$2" ]; then    
        cp "$1/$2" "$1/$2.old"
        echo "Backed up original $2 to $1/$2.old"
    fi
    ln -sf "$PWD/$3" "$1/$2"
}

set -x
set -e

# Configure .vimrc
link "$HOME" ".vimrc" "vimrc"

# Configure .gitconfig
link "$HOME" ".gitconfig" "gitconfig"

# Configure .zshrc
link "$HOME" ".zshrc" "zshrc"

# Create an auto updater for the dotfiles
if [ ! -f "$HOME/.update_dotfiles.sh" ]; then
    echo "#!/usr/bin/env sh" > $HOME/.update_dotfiles.sh
    echo "echo 'Updating dotfiles'" >> $HOME/.update_dotfiles.sh
    echo "cd $PWD" >> $HOME/.update_dotfiles.sh
    echo "git checkout master" >> $HOME/.update_dotfiles.sh
    echo "git pull" >> $HOME/.update_dotfiles.sh
    chmod +x $HOME/.update_dotfiles.sh
fi
