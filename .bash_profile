# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi
                              ## EXPORTS
export HISTSIZE=10000
export HISTCONTROL=ignoredups

export TERM="rxvt"
export EDITOR="emacsclient -nw -c -a \"\""
export ALTERNATE_EDITOR=""
export BROWSER="chromium"

export PATH=~/dotfiles/local/bin/:$PATH
export PATH=$PATH:/usr/local/android-studio/bin
export PATH=$PATH:~/.cabal/bin/
export PATH=$PATH:~/.local/bin/
export PATH=$PATH:/usr/local/heroku/bin/


# libglyrc that is used by the show_x script
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:~/.local/lib/

# Java | 
export JAVA_HOME=/usr/local/java/jdk8
export JDK_HOME=/usr/local/java/jdk8
export PATH=$JDK_HOME/bin/:$PATH
# Java Gui Toolkit fix for XMonad
export _JAVA_AWT_WM_NONREPARENTING=1 

# Android SDK
export ANDROID_HOME=~/Android/Sdk

# Princeton's algo course 
export CLASSPATH=~/.local/algs4/algs4.jar:$CLASSPATH
export CLASSPATH=~/.local/algs4/stdlib.jar:$CLASSPATH

export CLOUDSDK_PYTHON=/usr/bin/python2
