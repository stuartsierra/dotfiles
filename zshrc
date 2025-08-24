export PS1='%(?..%F{red}$?=%? )%f%F{blue}%3~%f %F{magenta}%#%f '

export EDITOR="${HOME}/dotfiles/bin/edit"
alias e="\$EDITOR"

alias gs="git status"
alias l="eza --classify"
alias ll="eza --all --classify --long"

# The following lines were added by compinstall
zstyle ':completion:*' completer _complete _ignored
zstyle :compinstall filename '/Users/sandra/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
