# Created by newuser for 5.0.7
autoload -Uz compinit promptinit
compinit
promptinit
prompt adam2
#History search
[[ -n "${key[PageUp]}" ]] && bindkey "${key[PageUp]}" history-beginning-search-backward
[[ -n "${key[PageDown]}" ]] && bindkey "${key[PageDown]}" history-beginning-search-forward
HISTFILE=~/.histfile
HISTSIZE=300
SAVEHIST=300
archey3
alias update=' sudo pacman -Syu'
alias sync=' sudo pacman -Syy'
alias i3='  vim ~/.i3/config'
alias i3status=' vim ~/.i3status.conf'
alias awesome=' vim ~/.config/awesome/rc.lua'
alias theme=' sudo vim /usr/share/awesome/themes/zenburn/theme.lua'
alias xmonad=' vim ~/.xmonad/xmonad.hs'
alias xin='vim .xinitrc'
alias add=' sudo pacman -S'
alias nitro=' nitrogen /home/mike/wallpapers'
alias remove=' sudo pacman -Rsn'
alias search='pacman -Ss'
alias orph='sudo pacman -Rns $(pacman -Qqtd)'
alias xre=' xrdb -load .Xresources'
alias clock=' tty-clock -f %m/%d/%y c'
alias news='newsbeuter'
alias music='mocp'
alias color='~/colorscheme.sh'
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
