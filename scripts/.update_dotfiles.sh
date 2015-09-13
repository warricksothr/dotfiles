#!/usr/bin/env sh

# Need to move into the git directory to perform the checks
opwd=$PWD
cd @GIT_DIR@

# Update the status of the origin
git remote update origin

# Check if we need to update at all
# Kindly borrowed from http://stackoverflow.com/questions/3258243/git-check-if-pull-needed
LOCAL=$(git rev-parse @)
REMOTE=$(git rev-parse @{u})
BASE=$(git merge-base @ @{u})

# All good, no need to update
if [ $LOCAL = $REMOTE ]; then
    echo "Up-to-date"
    exit 0
# Time to update to the remote branch
elif [ $LOCAL = $BASE ]; then
    echo "Need to pull"
    echo 'Updating dotfiles'
    git checkout master
    git pull
    git submodule update
# Local changes exist, we need to push these before we can cleanly update
elif [ $REMOTE = $BASE ]; then
    echo "Local changes, need to push before updating."
    exit 1
# Divereged branches, this will need a manual cleanup to fix
else
    echo "Error: Diverged Branches. Resolve Manually"
    exit 1
fi

# Return to where we started incase this was sourced from another script
cd $owpd
