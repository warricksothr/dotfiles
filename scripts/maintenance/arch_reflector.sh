#!/usr/bin/env bash

set -x
set -e

# Create a backup of the mirror list
mkdir -p $HOME/backup
cp -vf /etc/pacman.d/mirrorlist $HOME/backup/mirrorlist-$(date +%Y-%m-%d).bak

sudo curl -o /etc/pacman.d/mirrorlist https://www.archlinux.org/mirrorlist/all/
sudo reflector --verbose --country 'United States' -l 20 -p http --sort rate --save /etc/pacman.d/mirrorlist
