# Created by newuser for 5.0.2
autoload -Uz compinit promptinit
compinit
promptinit
prompt redhat
#History search
[[ -n "${key[PageUp]}"   ]]  && bindkey  "${key[PageUp]}"    history-beginning-search-backward
[[ -n "${key[PageDown]}" ]]  && bindkey  "${key[PageDown]}"  history-beginning-search-forward
HISTFILE=~/.histfile
HISTSIZE=300
SAVEHIST=300
archey3
