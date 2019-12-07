# Terminal colors
        RED="\[\033[0;31m\]"
     ORANGE="\[\033[0;33m\]"
     YELLOW="\[\033[0;33m\]"
      GREEN="\[\033[0;32m\]"
       BLUE="\[\033[0;34m\]"
  LIGHT_RED="\[\033[1;31m\]"
LIGHT_GREEN="\[\033[1;32m\]"
      WHITE="\[\033[1;37m\]"
 LIGHT_GRAY="\[\033[0;37m\]"
 COLOR_NONE="\[\e[0m\]"

# Used later (maybe in .bash_local) to determine if this is an
# interactive shell; see http://www.gnu.org/software/bash/manual/html_node/Is-this-Shell-Interactive_003f.html
case "$-" in
*i*)	INTERACTIVE_SHELL="true" ;;
*)	: ;;
esac

# Basic environment
export TERM=xterm-256color
export EDITOR=/usr/bin/nano
export HISTCONTROL=ignoreboth

# Prompt: see http://bashrcgenerator.com/
export PROMPT_DIRTRIM=3
export PS1="\[\033[38;5;2m\]\h:\[\033[38;5;4m\]\w\[\033[38;5;15m\] \[\033[38;5;5m\]\\$ \[\033[00m\]"

# Go language; https://golang.org/doc/code.html#GOPATH
export GOPATH="${HOME}/go"

export PATH="/Users/stuart/bin:/Users/stuart/.jenv/bin:/Users/stuart/.local/bin:/usr/local/sbin:/usr/local/bin:/opt/local/bin:/usr/local/opt/python3/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Library/TeX/texbin:/opt/X11/bin:/Users/stuart/go/bin"

# My aliases
if [[ "$(uname)" == "Darwin" ]]; then
    alias l="ls -FG"
    alias ls="ls -FG"
    alias ll="ls -lhFG"
    alias la="ls -ahFG"
    alias lal="ls -lahFG"
    alias d="pwd && echo && ls -FG"
else
    alias l="ls --color -F"
    alias ls="ls --color -F"
    alias ll="ls --color -lhF"
    alias la="ls --color -ahF"
    alias lal="ls --color -lahF"
    alias d="pwd && echo && ls --color -F"
fi

alias rm='rm -i'

alias beep="echo -e '\a'"

alias pgrep='ps aux | grep'

alias gs='git status'

# for Perl5 / CPAN
if [ -e /opt/local/lib/perl5 ]; then
    export PERL5LIB="/opt/local/lib/perl5/5.8.8:/opt/local/lib/perl5/site_perl/5.8.8:/opt/local/lib/perl5/vendor_perl/5.8.8"
fi
    
if which open > /dev/null; then
    alias e="open -a Emacs.app"
elif which emacs > /dev/null; then
    alias e="emacs"
else
    alias e="nano"
fi

# Local system stuff
if [ -e ~/.bash_local ]; then
    source ~/.bash_local
fi

# Git autocompletion
if [ -f ~/.git-completion.bash ]; then
    source ~/.git-completion.bash
fi

# Git helper functions
function git-delete-merged-branches() {
    git branch --merged | grep -v "\*" | grep -v master | xargs -n 1 git branch -d
    git remote prune origin
}

# Java on OS X
if [[ -f /usr/libexec/java_home ]]; then
    export JAVA_HOME="$(/usr/libexec/java_home)"
fi

## 'pass' Password Manager; http://www.zx2c4.com/projects/password-store/
if [[ -e /usr/local/etc/bash_completion.d/password-store ]]; then
    source /usr/local/etc/bash_completion.d/password-store
fi
if [[ -e /usr/local/etc/bash_completion.d/pass ]]; then
    source /usr/local/etc/bash_completion.d/pass
fi

## AWS command line interface
if ( which aws_completer > /dev/null ); then
    complete -C aws_completer aws
fi

## 'tag' wrapper for 'ag'
# See https://github.com/aykamko/tag
export TAG_CMD_FMT_STRING="emacsclient -n +{{.LineNumber}} {{.Filename}}"
if hash ag 2>/dev/null; then
  tag() { command tag "$@"; source ${TAG_ALIAS_FILE:-/tmp/tag_aliases} 2>/dev/null; }
  alias ag=tag
fi

# Re-acquire forwarded SSH key
# from http://tychoish.com/rhizome/9-awesome-ssh-tricks/
function ssh-reagent {
    for agent in /tmp/ssh-*/agent.*; do
        export SSH_AUTH_SOCK=$agent
        if ssh-add -l 2>&1 > /dev/null; then
            echo Found working SSH Agent:
            ssh-add -l
            return
        fi
    done
    echo Cannot find ssh agent - maybe you should reconnect and forward it?
}

if [ -d ~/src/clj/clojurescript ]; then
    export CLOJURESCRIPT_HOME=~/src/clj/clojurescript
fi

# iterm2 shell integration; see https://iterm2.com/documentation-shell-integration.html
if [ -e ~/.iterm2_shell_integration.bash ]; then
    source ~/.iterm2_shell_integration.bash
fi

# jenv: Java version manager, http://www.jenv.be/
if which jenv > /dev/null; then
    eval "$(jenv init -)"
fi

# 'z' fast directory switcher
if [ -e /usr/local/etc/profile.d/z.sh ]; then
    source /usr/local/etc/profile.d/z.sh
fi

# nvm: Node.js version manager, https://github.com/creationix/nvm
export NVM_DIR="$HOME/.nvm"
[ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/usr/local/opt/nvm/etc/bash_completion" ] && . "/usr/local/opt/nvm/etc/bash_completion"  # This loads nvm bash_completion
