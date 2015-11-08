# choose window-manager
wm=xmonad

#xrandr --output LVDS --mode 1280x768
[[ -f ~/.Xresources ]] && xrdb  ~/.Xresources

# start xscreensaver
xscreensaver -no-splash &

# start pulseaudo
start-pulseaudio-x11

# emacs
emacs --name MyEmacs &

# disable bell
xset -b

# start power monitor 
~/dotfiles/local/bin/power_info &

# start a composite manager
xcompmgr -Cc -r 8 &

case $wm in
	awesome)
		# start awesome
		awesome
		;;
	xmonad)
		# set cursor
		xsetroot -cursor_name left_ptr

		# wallpaper
		feh --bg-fill ~/dotfiles/background.png

        # start with the touchpad off
        ~/dotfiles/local/bin/tflip 

		# start xmonad | /usr/bin/bash so that ~/.bashrc is source and env set up!
	    /usr/bin/bash --login -c xmonad &> /dev/null
		;;
	*)
		;;
esac

