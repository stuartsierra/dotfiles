function backup {
    local file="$1"
    if [[ -L "$file" ]]; then
        rm "$file"
    elif [[ -e "$file" ]]; then
        mv "$file" "$file.bak"
    fi
}

function link_with_backup {
    local filename="$1"
    local source="$DOTFILES/$filename"
    local target="$HOME/$filename"
    backup "$target"
    ln -s "$source" "$target"
}

function update_submodules {
    git submodule init
    git submodule update
}

function install_relevance_etc {
    backup "$HOME/.relevance-etc"
    ln -s "$DOTFILES/submodules/relevance/etc" "$HOME/.relevance-etc"
}

function install_org_mode {
    (
        mkdir -p "$DOTFILES/.emacs.d/vendor"
        cd "$DOTFILES/.emacs.d/vendor"
        if [ ! -d org-mode ]; then
            wget -O org-mode-master.zip \
                https://github.com/stuartsierra/org-mode/archive/master.zip
            unzip org-mode-master.zip
            mv org-mode-master org-mode
        fi
        cd org-mode
        make
    )
}

# SSH autocomplete depends on ~/.ssh/config and known_hosts existing
function create_ssh_config {
    mkdir -p "$HOME/.ssh"
    for file in config known_hosts; do
        if [ ! -e "$HOME/.ssh/$file" ]; then
            touch "$HOME/.ssh/$file"
        fi
    done
}

function unset_git_user {
    for var in user.name user.email user.initials; do
        if ( git config --list --global | grep "$var" &> /dev/null ); then
            git config --global --unset "$var"
        fi
    done
}
