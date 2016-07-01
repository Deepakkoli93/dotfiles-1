#!/usr/bin/env bash
# Script to replicate my environment on ArchLinux
set -o nounset
set -o errexit

function install-packages() {
    # Package list
    # to be organized properly #TODO
    PACKAGES="git time ngrep
		  vim tmux emacs markdown
		  xorg-server xorg-xinit xorg-server-utils  xorg-xinput
		  xmonad dzen2 i3status xcompmgr dmenu xdotool xmonad-contrib wmctrl
		  weechat rxvt-unicode urxvt-perls
		  sdcv scrot xclip zathura feh youtube-dl 
		  mpc mpd ncmpcpp
		  pulseaudio pulseaudio-alsa ponymix pavucontrol 
		  mutt w3m 
		  gtk-theme-switch2 gtk-chtheme 
		  dosfstools dos2unix 
		  recordmydesktop 
		  openssh networkmanager network-manager-applet
		  linux-manpages tree 
		  gcc ghc
		  cabal-install 
		  python-pygments python2-pygments
          espeak fortune-mod
          glyr"


    # Install packages
    sudo  pacman -S  ${PACKAGES}

    # install dropbox
    read -p 'Do you want to install Dropbox now(y/N)? ' choice
    if [[ $choice = 'y' ]] ; then
	    echo 'Installing Dropbox...'
	    cd ~ && wget -O - "https://www.dropbox.com/download?plat=lnx.x86_64" | tar xzf -
    fi

    cd ${cur_dir}

    echo -e "\n\nDone!!"

    echo 'You may want to install infinality font patches now.'

}

function setup-dotfiles() {
    # Now clone the dotfiles repo
    cur_dir=`pwd`
    cd $HOME

    if [[ ! -d ~/dotfiles ]]; then
        echo "--> Cloning dotfiles repo."
        git clone https://github.com/narendraj9/dotfiles.git
    else
        echo "--> ~/dotfiles already exists."
    fi
    
    echo '--> Start making symlinks.'

    if [[ ! -d ~/.ssh ]] ; then
	    mkdir ~/.ssh
    fi

    # Start symlinking | Plumbing
    ## terminal
    ln -sb dotfiles/.bashrc .
    ln -sb dotfiles/.bash_profile .
    ln -sb dotfiles/.tmux.conf .

    ## misc
    ln -sb dotfiles/.gitconfig .
    ln -sb ~/dotfiles/.ssh/config ~/.ssh/config 

    # stardict dictionaries 
    [[ -d .stardict ]] && mv .stardict .startdict~
    ln -sb ~/dotfiles/.startdict .

    ## X server
    ln -sb dotfiles/.xinitrc .
    ln -sb ~/dotfiles/.Xresources .

    ## xmonad
    ln -sb dotfiles/.xmonad .
    ln -sb ~/dotfiles/.i3status.conf .

    # emacs
    [[ -d .emacs.d ]] && mv .emacs.d .emacs.d~
    ln -sb dotfiles/.emacs.d .

    # vim
    ln -sb dotfiles/.vimrc .
    [[ -d .vim ]] && mv .vim .vim~
    ln -sb dotfiles/.vim .
    
    # ghci
    ln -sb dotfiles/.ghci .

    echo '--> Done '

}

# Do the real work now.
read -p 'Install archlinux packages? (y/N) ' choice
if [[ $choice =~ ^(y) ]]; then
    install-packages
fi

# Always setup symbolic links.
setup-dotfiles
