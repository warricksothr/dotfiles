#!/usr/bin/env bash

#
# A script to convert the local system to use my dotfiles my local dotfiles over
# the provided ones already on the system. This script will backup any of the
# existing dot files, before linking to the ones in this repository.
#
# This script must be able to operate cleanly on an already bootstraped environment
# so as to update the system cleanly to new versions of the dotfiles repository.
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

copy_dir() {
    # Only make a backup if an existing directory is there and is not a link
    if [ -d "$1/$2" ] || [ -L "$1/$2" ]; then
        # Allow passing true as third param to do backups
        backup=true
        if [ ! -z "$3" ]; then
            backup=$4
        fi
        if $backup; then
            mv "$1/$2" "$1/$2.old"
            echo "Backed up original $2 to $1/$2.old"
        else
            rm -r "$1/$2"
            echo "removed original $2"
        fi
    fi
    # Should probably only do this is the source is newer
    # Remove the old and copy the new
    cp -r "$3" "$1/$2"
}

# Escape the path so we can use it in sed
escape_path() {
    local safe="$(echo $1 | sed 's/\//\\\//g')"
    echo $safe
}

# Clone a git repository into a location 
clone_git_repo() {
    local target=$1
    local repo=$2
    if [ ! -d "$target" ]; then
        mkdir -p "$target"
        echo "Cloning [$repo] into [$target]"
        git clone "$repo" "$target"
    else
        # Update the repo otherwise
        echo "Updating [$repo]"
        cur_dir=$PWD
        cd "$target"
        git checkout .
        git pull
        cd $cur_dir
    fi
}

set -x
set -e

# Need to get the source for the script
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
    TARGET="$(readlink "$SOURCE")"
    if [[ $TARGET == /* ]]; then
        echo "SOURCE '$SOURCE' is an absolute symlink to '$TARGET'"
        SOURCE="$TARGET"
    else
        DIR="$( dirname "$SOURCE" )"
        echo "SOURCE '$SOURCE' is a relative symlink to '$TARGET' (relative to '$DIR')"
        SOURCE="$DIR/$TARGET" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
    fi
done
echo "SOURCE is '$SOURCE'"
RDIR="$( dirname "$SOURCE" )"
DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
if [ "$DIR" != "$RDIR" ]; then
    echo "DIR '$RDIR' resolves to '$DIR'"
fi
echo "DIR is '$DIR'"

# Step into the directory where thes script is
cd $DIR

# Store the path to the root directory and the reference home directory
GIT_DIR="$(echo `git rev-parse --show-toplevel`)"

cd $GIT_DIR

# Home directory in the git directory
DOTFILES_DIR="$GIT_DIR"
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
    copy_dir $VIM_BUNDLE $plugin_name $plugin false
done

# Configure .gitconfig
link "$HOME" ".gitconfig" "$DOTFILES_HOME/.gitconfig"

# Configure .zshrc
link "$HOME" ".zshrc" "$DOTFILES_HOME/.zshrc"

clone_git_repo $HOME/.tmux/plugins/tpm https://github.com/tmux-plugins/tpm.git
clone_git_repo $HOME/.tmux/plugins/tmux-sensible https://github.com/tmux-plugins/tmux-sensible.git
clone_git_repo $HOME/.tmux/plugins/tmux-resurrect https://github.com/tmux-plugins/tmux-resurrect.git

# Configure .tmux.conf
link "$HOME" ".tmux.conf" "$DOTFILES_HOME/.tmux.conf"

cd $GIT_DIR

# Create an auto updater for the dotfiles
SOURCE_MD5="$(md5sum scripts/.update_dotfiles.sh | tr -s ' ' | cut -d ' ' -f 1)"
WRITE_NEW_UPDATER=false
# If it doesn't exist, we need to create it
if [ ! -f "$HOME/.update_dotfiles.sh" ]; then
    WRITE_NEW_UPDATER=true
else
    # Now we need to check if the md5's don't match
    CURRENT_MD5="$(cat $HOME/.update_dotfiles.sh | tail -n 1 | tr -s ' ' | cut -d ' ' -f 2)"
    # No hash, we need to update the file
    if [ -z "$CURRENT_MD5" ]; then
        WRITE_NEW_UPDATER=true
    else
        # need to compare the hashes, if they don't match we need to update
        if [ "$CURRENT_MD5" != "$SOURCE_MD5" ]; then
            WRITE_NEW_UPDATER=true
        fi
    fi
fi

if $WRITE_NEW_UPDATER; then
    SAFE_PATH="$(escape_path $GIT_DIR)"
    cat "scripts/.update_dotfiles.sh" | sed "s/@GIT_DIR@/$SAFE_PATH/" > $HOME/.update_dotfiles.sh
    echo "# $SOURCE_MD5" >> $HOME/.update_dotfiles.sh
    chmod +x $HOME/.update_dotfiles.sh
fi
