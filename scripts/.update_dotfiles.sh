#!/usr/bin/env sh
echo 'Updating dotfiles'
cd @GIT_DIR@
git checkout master
git pull
git submodule update
