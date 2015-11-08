# .bashrc

# source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# start X automatically at login
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx

# auto-complete sudo and man pages
complete -cf sudo man

                              ## PRIMARY PROMPT
# select a prompt symbol for this terminal.
# ϗ (varkappa) is cooler than the rest so let's make it twice probable.
sym_list=(ϗ ζ ξ χ λ ϑ Σ λ ϗ)
sym_len=${#sym_list[@]}
sym_now=${sym_list[$((${RANDOM} % sym_len))]}

# proxies
host_list=(127.0.0.1)
proxy_port=3128

# If on a remote machine, make changes to differentiate it from local machine
if [[ ! ${HOSTNAME} = 'vicarie' ]]; then
    export PS1='(\[\e[1;31m\]\W\[\e[m\])\[\e[1;32m\] ${sym_now} \[\e[m\]> '
else
    export PS1='(\[\e[1;31m\]\W\[\e[m\])\[\e[1;32m\] ${sym_now} \[\e[m\]'
fi

 							## ALIASES
alias ..='cd ..'
alias vi='vim'
alias em='emacsclient -c -a ""'
alias cp='cp -v'
alias rm='rm -i'
alias mv='mv -i'
alias sudo='sudo -E'
alias kman='man -s 9'
alias R_repl="rlwrap R"
alias uzbl='uzbl-browser'
alias ls='ls --color=auto -F'
alias suspend='systemctl suspend'
alias feh='feh -B black --keep-zoom-vp --auto-zoom'
alias myspace='cd /datastore/Documents/myspace'
alias node_repl="node -e \"require('repl').start({ignoreUndefined: true})\""
alias nokia_log='tail -f /tmp/dial-nokia.log'

                             ## FUNCTIONS

# turn off backlight
function turn-backlight-off () 
{
    echo 'Turning backlight off in a second.'
    sleep 1 && xset dpms force off
}

# mount media at /media/devices
# assuming /media/devices exists
function mnt()
{
	sudo mount -o uid=nj,gid=nj,rw $1 /media/devices
	[[ $? -eq 0 ]] && cd /media/devices
}

# corresponding umount function
function umnt()
{
	sudo umount /media/devices
}

# connect to a projector
# connect-projector <projector-id>
connect-projector()
{
	xrandr --output $1 --auto --same-as LVDS
}


# send a command into the background suppressing
# outputs to terminal
back()
{
	"$@" &>/dev/null &
}

# start an application with proxy env variables set
export-proxy() {
	export http_proxy=${host_list[$1]}:${proxy_port}
	export https_proxy=${http_proxy}
}

# remind me of something after some time
# usage: remindme <time> <text>
remind-me()
{
    sleep $1 && zenity --info --text "$2" &
}

share-desktop-readonly() {
    x0vncserver -AlwaysShared -PasswordFile /home/nj/.vnc/passwd -AcceptKeyEvents=0 -AcceptPointerEvents=0
}

# setup every thing for work
bootstrap () {
    emacs &
    chromium &
    offlinemap &
}

open-tcp-port() {
    port=${1}
    sudo iptables -A TCP -p tcp --dport ${port} -j ACCEPT
}

open-udp-port() {
    port=${1}
    sudo iptables -A UDP -p tcp --dport ${port} -j ACCEPT
}

stockholm() {
    TZ="Europe/Stockholm" date
    date
}

							## MISC WORKAROUNDS
# prevent gnome-ssh-askpass from popping up when pushing to github
unset SSH_ASKPASS

