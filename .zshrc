#-*- mode: sh -*-

if [ -d ~/.oh-my-zsh ]; then
    export ZSH=~/.oh-my-zsh
    plugins=(git bundler virtualenvwrapper) . $ZSH/oh-my-zsh.sh
fi
