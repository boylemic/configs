	# Created by newuser for 5.0.7# Created by newuser for 5.0.2
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
alias i3=' sudo vim ~/.i3/config'
alias i3status=' sudo vim ~/.i3status.conf'
alias awesome=' sudo vim ~/.config/awesome/rc.lua'
alias theme=' sudo vim /usr/share/awesome/themes/default/theme.lua'
alias xin=' sudo vim .xinitrc'
alias add=' sudo pacman -S'
alias nitro=' nitrogen /home/mike/wallpapers'
