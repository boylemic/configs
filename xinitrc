#!/bin/sh
#
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
sleep 2 && nitrogen --restore &
urxvtd -q -o -f &
# exec gnome-session
# exec startkde
# exec startxfce4
exec i3
#exec mate-session
# ...or the Window Manager of your choice
