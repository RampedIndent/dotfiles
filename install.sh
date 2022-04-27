#!/bin/bash
UBUNTU_PACKEGES="zsh exa screen emacs aspell-en dvipng texlive-latex-extra cmake xclip"
ARCH_PACKEGES="zsh exa screen emacs aspell-en texlive-bin texlive-latexextra cmake xclip"
UI_PACKAGES="picom polybar nitrogen syncthing"
PROD_PACKAGES="blender inkscape krita"
DOTFILES_DIR=~/.dotfiles
#export DOTFILES_DIR="/home/rampedindent/.dotfiles"

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
echo $DISTRO
unset UNAME
echo $DOTFILES_DIR
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
    sudo apt install $UBUNTU_PACKAGES

    sudo apt install $UI_PACKAGES

fi


if [ "$DISTRO" == "ArcoLinux" ]; then
    echo "Updating to newest Arch Version"
    sudo pacman -Syu
    echo "Arch packages are upto date"
    echo "Installing git and stow"
    sudo pacman -S git stow --needed
    # Make Sure Dotfiles dir is up to date and installed
    echo "Making Sure Dotfiles dir is up to date and downloaded"
    if [ -d "$DOTFILES_DIR" ]; then
        echo "Updating Dotfiles"
        git pull 
    else
        echo "Cloning Dotfiles"
        git clone https://github.com/RampedIndent/dotfiles.git $DOTFILES_DIR
    fi
    echo "Changing to Dotfiles Dir"
    cd $DOTFILES_DIR
    echo "Using stow to create symbolic links for the items in the Dotfiles Dir"
    stow .
    echo "Installing Terminal Packages"
    sudo pacman -S $ARCH_PACKEGES --needed
    echo "Installing UI Packages"
    sudo pacman -S $UI_PACKAGES --needed

fi

OMZ_DIR=~/.oh-my-zsh
if ! [ -d "$OMZ_DIR" ]; then
    echo "Installing Oh My ZSH"
    sh -c "$(wget -O- https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" 
fi

P10K_DIR=~/powerlevel10k
if ! [ -d "$P10K_DIR" ]; then
    echo "Installing zsh Powerline10k"
    git clone --depth=1 https://github.com/romkatv/powerlevel10k.git ~/powerlevel10k
fi


#if [ -d "~/powerline10k" ]; then
#    echo "Installing zsh Powerline10k"
#    git clone --depth=1 https://github.com/ryanoasis/nerd-fonts.git ~/nerdfonts
#fi

FILE=~/.ssh/id_$HOSTNAME
if [ -f "$FILE" ]; then
    echo "$FILE exists."
else 
    echo "$FILE does not exist."
    ssh-keygen -t ed25519 -C "RampedIndent@gmail.com" -f $FILE
    echo "Run to check if ssh-agent is running" 
    echo "eval \"\$(ssh-agent -s)\""
    echo "ssh-add $FILE"
    echo "Remember to add key to github https://github.com/settings/keys"
fi
