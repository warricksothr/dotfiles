#!/usr/bin/env sh
echo 'Updating dotfiles'
cd /home/sothr/dotfiles
git checkout master
git pull
git submodule update
