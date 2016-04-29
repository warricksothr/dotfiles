#!/usr/bin/env bash

#
# A script to convert the local system to use my dotfiles my local dotfiles over
# the provided ones already on the system. This script will backup any of the
# existing dot files, before linking to the ones in this repository.
#
# This script must be able to operate cleanly on an already bootstraped environment
# so as to update the system cleanly to new versions of the dotfiles repository.
#

BASEDIR=$(dirname $0)

# Source the tools script
. ${BASEDIR}/tools.sh

#set -x
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

# Configure emacs
EMACS_CONFIG_HOME=$HOME/.emacs.d
mkdir -p $EMACS_CONFIG_HOME
emacs_configs=($(ls -d $DOTFILES_HOME/.emacs.d/*))
for emacs_config in ${emacs_configs[@]}; do
    parts=($(echo "$emacs_config" | tr '/' ' '))
    emacs_config_name=${parts[-1]}
    if [ -d $emacs_config ]; then
        link_dir $EMACS_CONFIG_HOME $emacs_config_name $emacs_config
    else
        link $EMACS_CONFIG_HOME $emacs_config_name $emacs_config
    fi
done

# Configure .gitconfig
link "$HOME" ".gitconfig" "$DOTFILES_HOME/.gitconfig"

# Configure .zshrc
link "$HOME" ".zshrc" "$DOTFILES_HOME/.zshrc"

clone_or_update_git_repo $HOME/.tmux/plugins/tpm https://github.com/tmux-plugins/tpm.git
clone_or_update_git_repo $HOME/.tmux/plugins/tmux-sensible https://github.com/tmux-plugins/tmux-sensible.git
clone_or_update_git_repo $HOME/.tmux/plugins/tmux-resurrect https://github.com/tmux-plugins/tmux-resurrect.git

# Configure .tmux.conf
link "$HOME" ".tmux.conf" "$DOTFILES_HOME/.tmux.conf"

# Copy example ssh config
copy "$HOME" ".ssh/config" "$DOTFILES_HOME/.ssh/config" false false

# Copy yaourtrc config
link "$HOME" ".yaourtrc" "$DOTFILES_HOME/.yaourtrc"

# Copy the systemd user files from .config
SYSTEMD_CONFIG_HOME=$HOME/.config/systemd/user
mkdir -p $SYSTEMD_CONFIG_HOME
systemd_configs=($(ls -d $DOTFILES_HOME/.config/systemd/user/*.service))
for systemd_config in ${systemd_configs[@]}; do
    parts=($(echo "$systemd_config" | tr '/' ' '))
    systemd_config_name=${parts[-1]}
    copy $SYSTEMD_CONFIG_HOME $systemd_config_name $systemd_config true true
done

# Install/update rbenv and plugins
clone_or_update_git_repo $HOME/.rbenv https://github.com/sstephenson/rbenv.git
clone_or_update_git_repo $HOME/.rbenv/plugins/ruby-build https://github.com/sstephenson/ruby-build.git

# <-- EDIT/ADD ABOVE THIS
# This should be the Last section 
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
