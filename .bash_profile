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

export PATH=$PATH:~/.cabal/bin/
export PATH=~/dotfiles/local/bin/:$PATH
export PATH=$PATH:~/.local/bin/
export PATH=$PATH:/usr/local/heroku/bin/

# libglyrc that is used by the show_x script
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:~/.local/lib/

# Princeton's algo course 
export CLASSPATH=~/.local/algs4/algs4.jar:$CLASSPATH
export CLASSPATH=~/.local/algs4/stdlib.jar:$CLASSPATH

export CLOUDSDK_PYTHON=/usr/bin/python2

