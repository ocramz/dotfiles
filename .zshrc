#-*- mode: sh -*-

if [ -d ~/.oh-my-zsh ]; then
    export ZSH=~/.oh-my-zsh
    plugins=(git) . $ZSH/oh-my-zsh.sh
fi
