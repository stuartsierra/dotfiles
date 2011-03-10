# Basic environment
export PS1='\[\e]1;\]$(basename $(dirname $PWD))/\W\[\a\]\u@\h \W\$ '
export EDITOR=/usr/bin/nano

# My aliases
alias l="ls -FG"
alias ll="ls -lhFG"
alias la="ls -ahFG"
alias lal="ls -lahFG"
alias rm='rm -i'
alias d="pwd && echo & ls -FG"

# My path
export PATH=~/bin:~/.cljr/bin:$PATH

# for MacPorts
export PATH=/opt/local/bin:/opt/local/sbin:$PATH
export MANPATH=/opt/local/share/man:$MANPATH

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

alias swank="java -Dclojure.compile.path=target/classes -cp src/test/resources:src/test/clojure:src/main/resources:src/main/clojure:target/test-classes:target/classes:target/dependency/* clojure.main -e \"(require 'swank.swank) (swank.swank/start-repl)\""

# Local system stuff
if [ -e ~/.bash_local ]; then
    source ~/.bash_local
fi

# Relevance "etc" scripts
if [ -d ~/.relevance-etc ]; then
    export PATH=$PATH:~/.relevance-etc/scripts
    source ~/.relevance-etc/bash/git.sh
    source ~/.relevance-etc/bash/git_autocompletion.sh
    source ~/.relevance-etc/bash/git_installer.sh
    source ~/.relevance-etc/bash/git_prompt.sh
    source ~/.relevance-etc/bash/ssh_autocompletion.sh
fi

# cdargs
if [ -f /opt/local/etc/profile.d/cdargs-bash.sh ]; then
    source /opt/local/etc/profile.d/cdargs-bash.sh
fi

# RVM
[[ -s $HOME/.rvm/scripts/rvm ]] && source $HOME/.rvm/scripts/rvm