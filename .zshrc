#-*- mode: sh -*-

# oh-my-zsh

[[ ! -d ~/.oh-my-zsh ]] \
    && git clone http://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh

[[ -d ~/.oh-my-zsh ]] \
    && export ZSH=~/.oh-my-zsh \
    && ZSH_THEME=arrow plugins=(git) source $ZSH/oh-my-zsh.sh

# basics

alias l="ls -la"

each() {
    find=$1 ; shift
    for found in $(find $PWD -name $find); do
        dir=$(dirname $found)
        pushd "${dir}" >/dev/null
        echo $dir && $@
        popd >/dev/null
    done
}

# tmux

alias tmux="nocorrect tmux"
alias t="tmux attach || tmux"

function ts() { tmux -S /tmp/$@ }
function ta() { ts $1 attach }
function tk() { ts $1 kill-session }
function tl() { ps aux | grep tmux | egrep -v grep }

# emacs

[[ -f /Applications/Emacs.app ]] \
    && PATH=/Applications/Emacs.app/*/*/bin:$PATH

alias emacsclient="nocorrect emacsclient"
export EDITOR="emacsclient -t --alternate-editor=''"
alias e=$EDITOR

function ec() { e -s $@ }
function es() { emacs --daemon=$1 && ec $@ }
function ek() { ec $1 -e '(server-stop)' }
function el() { ps aux | egrep "[Ee]macs" | egrep -v grep }

# git

alias git="nocorrect git"
alias g=git

# haskell

[[ -d ~/.cabal/bin ]] \
    && PATH=~/.cabal/bin:$PATH \

alias cabal-dev="nocorrect cabal-dev"
alias c=cabal-dev

# erlang

alias rebar="nocorrect rebar"
alias r="rebar skip_deps=true"

eunit() {
    while inotifywait -e modify -r $PWD/src $PWD/test $PWD/include; do
        echo "\n\nRun: $(date)"
        r compile && r eunit
        echo "End: $(date)\n\n"
    done
}

# python

# FIXME oh-my-zsh prompt sucks
[[ -f ~/.python/bin/activate ]] \
    && source ~/.python/bin/activate

# ruby

[[ -d ~/.gem/ruby/1.9.1/bin ]] && PATH=~/.gem/ruby/1.9.1/bin:$PATH

[[ -d ~/.rbenv/bin ]] \
    && PATH=~/.rbenv/bin:$PATH \
    && eval "$(rbenv init -)"

# chef

alias knife="nocorrect knife"
alias k=knife

# vagrant

alias vagrant="nocorrect bundle exec vagrant"
alias v=vagrant

# nix

[[ -f /usr/local/etc/profile.d/nix.sh ]] \
    && source /usr/local/etc/profile.d/nix.sh
