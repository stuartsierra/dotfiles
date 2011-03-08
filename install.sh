#!/usr/bin/env bash

set -e

cd `dirname $0`
DOTFILES=`pwd`

function link_with_backup {
    local FILENAME=$1
    local SOURCE=$DOTFILES/$FILENAME
    local TARGET=$HOME/$FILENAME
    if [ -L $TARGET ]; then
        rm $TARGET
    elif [ -e $TARGET ]; then
        mv $TARGET $TARGET.bak
    fi
    ln -s $SOURCE $TARGET
}

function install_elpa {
    rm -rf ~/.emacs/elpa
    emacs -l $DOTFILES/install_elpa.el
}

link_with_backup .bashrc
link_with_backup .bash_profile
link_with_backup .emacs
link_with_backup .emacs.d

