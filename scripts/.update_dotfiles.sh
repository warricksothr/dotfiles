#!/usr/bin/env sh

# Need to move into the git directory to perform the checks
opwd=$PWD
GIT_DIR="@GIT_DIR@"
cd $GIT_DIR

# Update the status of the origin
# No need to print the result, this should be silent and invisible
git remote update origin 1>/dev/null

# Check if we need to update at all
# Kindly borrowed from http://stackoverflow.com/questions/3258243/git-check-if-pull-needed
LOCAL=$(git rev-parse @)
REMOTE=$(git rev-parse @{u})
BASE=$(git merge-base @ @{u})

# Exit if for some reason we don't get info from the remote.
if [ -z "$REMOTE" ]; then
    exit 0
fi

# All good, no need to update
if [ $LOCAL = $REMOTE ]; then
    # No need to declare anything while we do this
    exit 0
# Time to update to the remote branch
elif [ $LOCAL = $BASE ]; then
    echo "Updating dotfiles in: $PWD"
    git checkout master
    git pull
    git submodule update

    #Since we updated, we should execute the bootstrapper again.
    . $GIT_DIR/scripts/bootstrap.sh

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
