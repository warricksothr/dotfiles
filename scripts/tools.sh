#!/usr/bin/env bash

# Tools for the scripts

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

copy() {
    # Only make a backup if an existing directory is there and is not a link
    # Allow passing true as third param to do backups
    backup=true
    if [ ! -z "$4" ]; then
        backup=$4
    fi
    overwrite=false
    if [ ! -z "$5" ]; then
        overwrite=$5
    fi
    if [ -f "$1/$2" ] || [ -L "$1/$2" ]; then
        if $backup; then
            mv "$1/$2" "$1/$2.old"
            echo "Backed up original $2 to $1/$2.old"
        else
            if $overwrite; then
                rm -r "$1/$2"
                echo "removed original $2"
            fi
        fi
    fi
    # Should probably only do this is the source is newer
    # Remove the old and copy the new
    if $overwrite; then
        cp -r "$3" "$1/$2"
    fi
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
# Or update an existing repository
clone_or_update_git_repo() {
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
