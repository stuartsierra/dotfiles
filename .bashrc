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
export PS1="${BLUE}\h:\W \$${COLOR_NONE} "
export EDITOR=/usr/bin/nano

# Go language; https://golang.org/doc/code.html#GOPATH
export GOPATH="${HOME}/go"

# PATH munging
# See http://tldp.org/LDP/Bash-Beginners-Guide/html/sect_11_02.html
EGREP=$(which egrep)

pathmunge () {
    if ! echo $PATH | $EGREP -q "(^|:)$1($|:)" ; then
        if [ "$2" = "after" ] ; then
            PATH=$PATH:$1
        else
            PATH=$1:$PATH
        fi
    fi
}

pathmunge /sbin
pathmunge /usr/sbin
pathmunge /usr/local/sbin
pathmunge /bin
pathmunge /usr/bin
pathmunge /opt/local/bin
pathmunge /usr/local/bin
pathmunge "$HOME/bin"

pathmunge /opt/X11/bin after
pathmunge /usr/texbin after
pathmunge "$HOME/.relevance-etc/scripts" after
pathmunge "$GOPATH/bin" after
pathmunge /usr/local/opt/go/libexec/bin after

export PATH
unset pathmunge

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

alias fig='rlwrap lein figwheel'

# for Perl5 / CPAN
if [ -e /opt/local/lib/perl5 ]; then
    export PERL5LIB="/opt/local/lib/perl5/5.8.8:/opt/local/lib/perl5/site_perl/5.8.8:/opt/local/lib/perl5/vendor_perl/5.8.8"
fi
    
if [ -e /Applications/TrueCrypt.app/Contents/MacOS/TrueCrypt ]; then
    alias truecrypt="/Applications/TrueCrypt.app/Contents/MacOS/TrueCrypt -t"
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

# Relevance "etc" scripts
if [ -d ~/.relevance-etc ]; then
    # source ~/.relevance-etc/bash/git.sh
    # source ~/.relevance-etc/bash/git_prompt.sh
    source ~/.relevance-etc/bash/ssh_autocompletion.sh
fi

# Relevance pairing
if [ -e ~/sourcecode.tc ]; then
    alias sourcecode="truecrypt -t -k '' --protect-hidden=no $HOME/sourcecode.tc $HOME/src"
fi

# cdargs
if [ -f /opt/local/etc/profile.d/cdargs-bash.sh ]; then
    source /opt/local/etc/profile.d/cdargs-bash.sh
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

# Google Chrome Testing instances
function newchrome {
    local now=`date +%Y%m%d%H%M%S`
    local dir=/tmp/chrome$now
    cp -R ~/fresh-chrome "$dir"
    open -na 'Google Chrome' --args --user-data-dir="$dir"
}

## GPG Agent
## From http://sudoers.org/2013/11/05/gpg-agent.html
GPG_AGENT=$(which gpg-agent)
GPG_TTY=`tty`
export GPG_TTY
if [[ -f ${GPG_AGENT} && -e "${HOME}/.bash_gpg" ]]; then
    source "${HOME}/.bash_gpg"
fi

## 'pass' Password Manager; http://www.zx2c4.com/projects/password-store/
if [[ -e /usr/local/etc/bash_completion.d/password-store ]]; then
    source /usr/local/etc/bash_completion.d/password-store
fi

## AWS command line interface
if ( which aws_completer > /dev/null ); then
    complete -C aws_completer aws
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
