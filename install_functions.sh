function backup {
    local FILE=$1
    if [ -L $FILE ]; then
        rm $FILE
    elif [ -e $FILE ]; then
        mv $FILE $FILE.bak
    fi
}

function link_with_backup {
    local FILENAME=$1
    local SOURCE=$DOTFILES/$FILENAME
    local TARGET=$HOME/$FILENAME
    backup $TARGET
    ln -s $SOURCE $TARGET
}

function install_elpa {
    rm -rf $DOTFILES/.emacs.d/elpa
    emacs --script $DOTFILES/install_elpa.el
}

function update_submodules {
    git submodule init
    git submodule update
}

function install_relevance_etc {
    backup ~/.relevance-etc
    ln -s $DOTFILES/submodules/relevance/etc $HOME/.relevance-etc
}
