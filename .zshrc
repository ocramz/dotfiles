#-*- mode: sh -*-

if [ ! -d ~/.oh-my-zsh ]; then
    git clone --depth=1 git://github.com/dysinger/oh-my-zsh.git ~/.oh-my-zsh
fi

if [ -d ~/.oh-my-zsh ]; then
    export ZSH=~/.oh-my-zsh
    plugins=(git rbenv bundler) . $ZSH/oh-my-zsh.sh
fi
