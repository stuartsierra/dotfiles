export PS1='%(?..%F{red}$?=%? )%f%F{blue}%3~%f %F{magenta}%#%f '

alias gs="git status"
alias e="$EDITOR"
alias l="exa --classify"
alias ll="exa --all --classify --long"

# The following lines were added by compinstall
zstyle ':completion:*' completer _complete _ignored
zstyle :compinstall filename '/Users/sandra/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
