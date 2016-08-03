# dotfiles
Simple collection of my standard machine configuration files and scripts

# Purpose

Act as a central repository for the basic configuration I use for my development
and personal machines. Also include scripts for the basic setup for each of my
standard environments.

# Usage

scripts/bootstrap.sh is the script that does the basic linking of the dotfiles in the home directory along with the vim plugins

scripts/installers/install_tools_arch.sh is a script for arch linux to install the basic tools needed on my workstations

a ~/.update_dotfiles.sh script will be added to the home directory to automatically try and update the dotfiles git directory as soon as possible.

More To Be Added.

# Todo

- [x] Make the .update_dotfiles script smarter in how it updates
- [x] Work on emacs configuration
- [] Break features out into subscripts so that .zshrc can be reduced in size and just sub contract work out to the feature scripts
- [] Add support for additional environments

# License

The included scripts and configurations are licensed under the MIT license.
For additional information please see the LICENSE file in this directory.
