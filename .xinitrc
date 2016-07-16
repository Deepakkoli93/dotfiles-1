# choose window-manager
WM=xmonad
WALLPAPER_DIR=~/miscellany/assets/images/casper-david-friedrich/
POWER_INFO_BIN=~/dotfiles/local/bin/power_info
CONFIG_DIR=~/dotfiles/

#xrandr --output LVDS --mode 1280x768
[[ -f ~/.Xresources ]] && xrdb  ~/.Xresources

# start xscreensaver
xscreensaver -no-splash &

# start pulseaudo
start-pulseaudio-x11

# disable bell
xset -b

# start power monitor 
${POWER_INFO_BIN} &

# start a composite manager
xcompmgr -Cc -r 8 &

# start syndaemon
syndaemon -d -i 1.0

# start the notificaion daemon
dunst -config $CONFIG_DIR/.dunstrc &

case $WM in
	awesome)
		# start awesome
		awesome
		;;
	xmonad)
		# set cursor
		xsetroot -cursor_name left_ptr

		# randomly pick a wallpaper
		feh --bg-fill "$(find ${WALLPAPER_DIR} -type f | shuf -n 1)"

		# start xmonad | /usr/bin/bash so that ~/.bashrc is source and env set up!
	    /usr/bin/bash --login -c xmonad &> /dev/null
		;;
	*)
		;;
esac

