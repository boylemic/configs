# ~/.xinitrc
#
# Executed by startx (run your window manager from here)
if [ -d /etc/X11/xinit/xinitrc.d ]; then
for f in /etc/X11/xinit/xinitrc.d/*; do
[ -x "$f" ] && . "$f"
done
unset f
fi
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK
export GPG_AGENT_INFO
sleep 2 && nitrogen --restore &
urxvtd -q -o -f &
udiskie &
compton -b &
mpd &
redshift &
#nm-applet &
#volumeicon &
#dropbox &
#stalonetray &
#trayer --edge bottom --align right --SetDoc#kType true --SetPartialStrut true --expand true --widthtype percent --width 9 --transparent true --tint 0x000000 --height 12 &

autocutsel -fork &
autocutsel -selection PRIMARY -fork &
/usr/bin/start-pulseaudio-x11 &
#sxhkd &
# exec gnome-session
# exec startkde
# exec startxfce4
#exec i3
#exec awesome
#exec mate-session
#exec xmonad
exec herbstluftwm
#exec bspwm
# ...or the Window Manger of your choice
