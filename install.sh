#! /bin/bash

export UBUNTU_PACKEGES = "exa screen emacs polybar aspell-en"
export DOTFILES_DIR = "~\.dotfiles"

# Check if computer is ubuntu
# Determine OS platform
# https://askubuntu.com/a/459425
UNAME=$(uname | tr "[:upper:]" "[:lower:]")
# If Linux, try to determine specific distribution
if [ "$UNAME" == "linux" ]; then
    # If available, use LSB to identify distribution
    if [ -f /etc/lsb-release -o -d /etc/lsb-release.d ]; then
        export DISTRO=$(lsb_release -i | cut -d: -f2 | sed s/'^\t'//)
    # Otherwise, use release info file
    else
        export DISTRO=$(ls -d /etc/[A-Za-z]*[_-][rv]e[lr]* | grep -v "lsb" | cut -d'/' -f3 | cut -d'-' -f1 | cut -d'_' -f1)
    fi
fi
# For everything else (or if above failed), just use generic identifier
#[ "$DISTRO" == "" ] && export DISTRO=$UNAME
echo $(UNAME)
unset UNAME

if [ "$DISTRO" == "Ubuntu" ]; then
    # sudo apt install exa
    # sudo apt install screen
    echo "Updating to newest Ubuntu Version"
    #sudo apt install update-manager-core ubuntu-release-upgrader-core
    #sudo sed -i '/Prompt=lts/c\Prompt=normal' /etc/update-manager/release-upgrades
    #sudo apt update && sudo apt dist-upgrade
    echo "Ubuntu version upto date"
    echo "Installing git and stow"
    #sudo apt update && sudo apt install git stow
    # Make Sure Dotfiles dir is up to date and installed
    echo "Making Sure Dotfiles dir is up to date and downloaded"
    if [ -d "$DOTFILES_DIR" ]; then
	echo "Updating Dotfiles"
	git pull 
    else
	echo "Cloning Dotfiles"
	#git clone https://github.com/RampedIndent/dotfiles.git $DOTFILES_DIR
    fi
    echo "Changing to Dotfiles Dir"
    cd $DOTFILES_DIR
    echo "Using stow to create symbolic links for the items in the Dotfiles Dir"
    stow .
    
fi

    
