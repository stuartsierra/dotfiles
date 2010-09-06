# Basic environment
export PS1="\u@\h \W\$ "
export EDITOR=/usr/bin/nano

# My aliases
alias l="ls -FG"
alias ll="ls -lhFG"
alias la="ls -ahFG"
alias lal="ls -lahFG"
alias rm='rm -i'
alias d="pwd && echo & ls -FG"

# My path
export PATH=~/bin:$PATH

# for MacPorts
export PATH=/opt/local/bin:/opt/local/sbin:$PATH
export MANPATH=/opt/local/share/man:$MANPATH

# for Perl5 / CPAN
export PERL5LIB="/opt/local/lib/perl5/5.8.8:/opt/local/lib/perl5/site_perl/5.8.8:/opt/local/lib/perl5/vendor_perl/5.8.8"

# for Java JDK 1.6
export JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Versions/1.6/Home
alias java6=$JAVA_HOME/bin/java
alias javac6=$JAVA_HOME/bin/javac

alias truecrypt="/Applications/TrueCrypt.app/Contents/MacOS/TrueCrypt -t"

# Relevance "etc" scripts
source ~/src/relevance/etc/bash/git.sh
source ~/src/relevance/etc/bash/git_autocompletion.sh
source ~/src/relevance/etc/bash/git_installer.sh
source ~/src/relevance/etc/bash/git_prompt.sh
source ~/src/relevance/etc/bash/ssh_autocompletion.sh

# RVM
[[ -s $HOME/.rvm/scripts/rvm ]] && source $HOME/.rvm/scripts/rvm
