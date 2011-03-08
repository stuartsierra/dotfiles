#!/usr/bin/env bash

set -e

cd `dirname $0`
DOTFILES=`pwd`

source $DOTFILES/install_functions.sh

update_submodules
install_elpa

install_relevance_etc

link_with_backup .bashrc
link_with_backup .bash_profile
link_with_backup .emacs
link_with_backup .emacs.d
link_with_backup .gitconfig

backup ~/.relevance-etc
ln -s $DOTFILES/submodules/relevance/etc $HOME/.relevance-etc
