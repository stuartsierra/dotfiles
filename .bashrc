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


# Basic environment
export TERM=xterm-256color
export PS1="${BLUE}\W \$${COLOR_NONE} "
export EDITOR=/usr/bin/nano

# My aliases
if [ `uname` = "Darwin" ]; then
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

# My path
export PATH=~/bin:/usr/local/bin:/opt/local/bin:$PATH

# for MacPorts
export MANPATH=$MANPATH:/opt/local/share/man

# for PostgreSQL 8.4 installed via MacPorts
export PATH=$PATH:/opt/local/lib/postgresql84/bin

# for Perl5 / CPAN
if [ -e /opt/local/lib/perl5 ]; then
    export PERL5LIB="/opt/local/lib/perl5/5.8.8:/opt/local/lib/perl5/site_perl/5.8.8:/opt/local/lib/perl5/vendor_perl/5.8.8"
fi
    
# for Java JDK 1.6
if [ -e /System/Library/Frameworks/JavaVM.framework/Versions/1.6/Home ]; then
    export JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Versions/1.6/Home
    alias java6=$JAVA_HOME/bin/java
    alias javac6=$JAVA_HOME/bin/javac
fi

if [ -e /Applications/TrueCrypt.app/Contents/MacOS/TrueCrypt ]; then
    alias truecrypt="/Applications/TrueCrypt.app/Contents/MacOS/TrueCrypt -t"
fi

alias copy_music_to_droid="rsync --ignore-existing -avz ~/Music/iTunes/iTunes\ Media/Music/ /Volumes/ANDROID/Music"

alias copy_photos_from_droid="rsync --ignore-existing -avz /Volumes/NO\ NAME/DCIM/100MEDIA/ /Users/stuart/Pictures/my-photos/"

alias swank="java -Dclojure.compile.path=target/classes -cp src/test/resources:src/test/clojure:src/main/resources:src/main/clojure:target/test-classes:target/classes:target/dependency/* clojure.main -e \"(require 'swank.swank) (swank.swank/start-repl)\""

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
    export PATH=$PATH:~/.relevance-etc/scripts
    source ~/.relevance-etc/bash/git.sh
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
if [ -f /opt/local/etc/bash_completion ]; then
      source /opt/local/etc/bash_completion
fi

# JDK 7
function java7 {
    JAVA_HOME="/Library/Java/JavaVirtualMachines/openjdk-1.7-x86_64/Contents/Home"
    PATH="$JAVA_HOME/bin:$PATH"
    echo "Using Java 7 at $JAVA_HOME"
}

# Google Chrome Testing instances
function newchrome {
    local now=`date +%Y%m%d%H%M%S`
    local dir=/tmp/chrome$now
    cp -R ~/fresh-chrome "$dir"
    open -na 'Google Chrome' --args --user-data-dir="$dir"
}

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

# rbenv
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
