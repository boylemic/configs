# Created by newuser for 5.0.7
autoload -Uz compinit promptinit
compinit
promptinit
#prompt adam2
ZSH=/home/mike/.oh-my-zsh/
ZSH_THEME="gentoo"
#"bira" 
#"agnoster" 
#"powerline"
#lambda
#gentoo
#imajes
#kennethreitz
#History search
[[ -n "${key[PageUp]}" ]] && bindkey "${key[PageUp]}" history-beginning-search-backward
[[ -n "${key[PageDown]}" ]] && bindkey "${key[PageDown]}" history-beginning-search-forward
HISTFILE=~/.histfile
HISTSIZE=300
SAVEHIST=300
DISABLE_AUTO_UPDATE="true"
screenfetch

### ARCH ########
#alias update=' sudo pacman -Syu'
##alias remove=' sudo pacman -Rsn'
#alias search='pacman -Ss'
#alias orph='sudo pacman -Rns $(pacman -Qqtd)'
#alias sync=' sudo pacman -Syy'
#alias pac='sudo vim /etc/pacman.conf'
#alias pac='sudo vim /etc/pacman.conf'

#### WINDOWMANAGERS ####
alias eI='  vim ~/.i3/config'
alias eIS=' vim ~/.i3status.conf'
#alias eA=' vim ~/.config/awesome/rc.lua'
#alias eT=' sudo gedit /usr/share/awesome/themes/zenburn/theme.lua'
alias eX=' vim ~/.xmonad/xmonad.hs'
alias eXB=' vim ~/.xmobarrc'
alias eH=' vim ~/.config/herbstluftwm/autostart'
alias eP=' vim ~/.config/herbstluftwm/panel.sh'

### MISC ########
alias xin='vim .xinitrc'
alias add=' sudo emerge -av'
alias nitro=' nitrogen /home/mike/wallpapers'
alias xre=' xrdb -load ~/.Xresources'
alias clock=' tty-clock -f %m/%d/%y c'
alias news='newsbeuter'
alias music='ncmpcpp'
alias vid=' mpv dvdnav://'
alias color='~/colorscheme.sh'
alias net=' nmtui'
alias weather=' curl wttr.in'
alias slur='slurm -i wlo1'
alias home=' cd ~'
alias starwars='telnet towel.blinkenlights.nl'

#####SSH ######
alias gen='ssh mike@192.168.254.17'
alias rpi='ssh pi@192.168.254.29 -p1194'

#### Gentoo  ##########
alias HowLong='genlop -t'
alias OneShot='sudo emerge --oneshot portage'
alias Add='sudo emerge -av'
alias Dclean='sudo emerge --depclean --ask'
alias Rebuild='sudo revdep-rebuild -v'
alias Sync='time sudo emerge --sync'
alias S='eix'
alias I='time sudo emerge -av'
alias Uworld='time sudo emerge --ask --verbose --update --newuse --deep @world'
# alias Uworld-bdeps='time emerge --ask --newuse --update --deep --with-bdeps=y --keep-going @world'
alias Esync='sudo eix-sync'
alias Eupdate='sudo eix --update'
alias P='cd /etc/portage && sudo su'
alias U='cd /etc/portage/package.use && sudo su'
alias K=' cd /usr/src/linux && sudo su'
alias cdM='cd /etc/portage/package.mask && sudo su'
alias cdK='cd /etc/portage/package.accept_keywords && sudo su'
alias eM='sudo vim /etc/portage/make.conf'

## Portage
alias G='sudo watch genlop -unc'
alias F='sudo tail -f /var/log/emerge-fetch.log'
alias E='sudo tail -f /var/log/emerge.log'

## Zsh Stuff
alias eZ='vim ~/.zshrc'
alias Z='source ~/.zshrc'

## Vim Stuff
alias eV='vim ~/.vimrc'
alias e='vim'

[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
export LANG="en_US.utf8"
export LC_COLLATE="C"
source $ZSH/oh-my-zsh.sh
source /usr/share/zsh/site-contrib/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
