# Created by newuser for 5.0.7
autoload -Uz compinit promptinit
compinit
promptinit
#prompt adam2
ZSH=/usr/share/oh-my-zsh/
ZSH_THEME="bira"
#"bira" 
#"agnoster" 
#"powerline"
#lambda
#kennethreitz
#History search
[[ -n "${key[PageUp]}" ]] && bindkey "${key[PageUp]}" history-beginning-search-backward
[[ -n "${key[PageDown]}" ]] && bindkey "${key[PageDown]}" history-beginning-search-forward
HISTFILE=~/.histfile
HISTSIZE=300
SAVEHIST=300
DISABLE_AUTO_UPDATE="true"
archey3
#~/system.sh
alias update=' sudo pacman -Syu'
alias sync=' sudo pacman -Syy'
alias i3='  vim ~/.i3/config'
alias i3status=' vim ~/.i3status.conf'
alias awesome=' vim ~/.config/awesome/rc.lua'
alias theme=' sudo gedit /usr/share/awesome/themes/zenburn/theme.lua'
alias x=' vim ~/.xmonad/xmonad.hs'
alias herb=' vim ~/.config/herbstluftwm/autostart'
alias xin='vim .xinitrc'
alias add=' sudo pacman -S'
alias nitro=' nitrogen /home/mike/wallpapers'
alias remove=' sudo pacman -Rsn'
alias search='pacman -Ss'
alias orph='sudo pacman -Rns $(pacman -Qqtd)'
alias xre=' xrdb -load .Xresources'
alias clock=' tty-clock -f %m/%d/%y c'
alias news='newsbeuter'
alias music='ncmpcpp'
alias vid=' mpv dvdnav://'
alias color='~/colorscheme.sh'
alias pac='sudo vim /etc/pacman.conf'
alias net=' nmtui'
alias mirror=' sudo reflector --verbose -l 5 --sort rate --save /etc/pacman.d/mirrorlist'
alias weather=' curl wttr.in'
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
source $ZSH/oh-my-zsh.sh
