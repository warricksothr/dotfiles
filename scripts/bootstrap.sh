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
    ln -sf "$3" "$1/$2"
}

link_dir() {
    # Only make a backup if an existing directory is there and is not a link
    if [ -d "$1/$2" ] && [ ! -L "$1/$2" ]; then
        mv "$1/$2" "$1/$2.old"
        echo "Backed up original $2 to $1/$2.old"
    fi

    # Don't overwrite links
    if [ ! -L "$1/$2" ]; then
        echo "ln -sf $3 $1/$2"
        ln -sf "$3" "$1/$2"
    fi
}

set -x
set -e

# Step out of the script directory and into the main dotfiles directory
cd ..

# Store the path to the root directory and the reference home directory
DOTFILES_DIR="$PWD"
DOTFILES_HOME="$DOTFILES_DIR/home"

# Configure .vimrc
link "$HOME" ".vimrc" "$DOTFILES_HOME/.vimrc"

# Download pathogen if it doesn't exist already
VIM_AUTOLOAD=$HOME/.vim/autoload
mkdir -p $VIM_AUTOLOAD
if [ ! -f "$VIM_AUTOLOAD/pathogen.vim" ]; then
    curl -LSso $VIM_AUTOLOAD/pathogen.vim https://tpo.pe/pathogen.vim
fi

# Configure Pathogen Plugins
VIM_BUNDLE=$HOME/.vim/bundle
# Make the bundle directory and it's parents if they don't exist
mkdir -p $VIM_BUNDLE
# A list of vim plugins that are submodules
plugins=($(ls -d $DOTFILES_HOME/.vim/bundle/* | tr -s ' '))
#echo "${plugins[@]}"
# Loop through all of the plugins and install them as links in the bundle directory
for plugin in ${plugins[@]}; do
    parts=($(echo "$plugin" | tr '/' ' '))
    plugin_name=${parts[-1]}
    #echo "$VIM_BUNDLE $plugin_name $plugin"
    link_dir $VIM_BUNDLE $plugin_name $plugin
done

# Configure .gitconfig
link "$HOME" ".gitconfig" "$DOTFILES_HOME/.gitconfig"

# Configure .zshrc
link "$HOME" ".zshrc" "$DOTFILES_HOME/.zshrc"

# Create an auto updater for the dotfiles
if [ ! -f "$HOME/.update_dotfiles.sh" ]; then
    echo "#!/usr/bin/env sh" > $HOME/.update_dotfiles.sh
    echo "echo 'Updating dotfiles'" >> $HOME/.update_dotfiles.sh
    echo "cd $PWD" >> $HOME/.update_dotfiles.sh
    echo "git checkout master" >> $HOME/.update_dotfiles.sh
    echo "git pull" >> $HOME/.update_dotfiles.sh
    echo "git submodule init" >> $HOME/.update_dotfiles.sh
    echo "git submodule update" >> $HOME/.update_dotfiles.sh
    chmod +x $HOME/.update_dotfiles.sh
fi
