#-*- mode: sh -*-

# oh-my-zsh

[[ ! -d ~/.oh-my-zsh ]] \
    && git clone http://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh

[[ -d ~/.oh-my-zsh ]] \
    && export ZSH=~/.oh-my-zsh \
    && ZSH_THEME=arrow \
    plugins=(git) \
    source $ZSH/oh-my-zsh.sh

# basics

export PAGER=less

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

export GIT_EDITOR=nano

# screencasting

screencast() {
    cvlc screen:// \
        --screen-fps=12 \
        --input-slave=alsa:// \
        --sout "\
#transcode{venc=x264,vcodec=h264,fps=12,vb=640,acodec=mp3,channels=1,ab=64}\
:std{access=file,mux=mp4,dst=screencast-2012-03-22-1332407228.avi}\
"
}

# haskell

[[ -d ~/.cabal/bin ]] \
    && PATH=~/.cabal/bin:$PATH \

alias c=cabal-dev

# erlang

alias rebar="nocorrect rebar"
alias r="rebar skip_deps=true"

export ERL_LIBS=~/src/proper

eunit() {
    while inotifywait -e modify -r $PWD/src $PWD/test $PWD/include; do
        echo "\n\nRun: $(date)"
        r compile && r eunit
        echo "End: $(date)\n\n"
    done
}

# python

# FIXME oh-my-zsh prompt sucks
#[[ -f ~/.python/bin/activate ]] \
#    && source ~/.python/bin/activate

# ruby

[[ -d ~/.gem/ruby/1.9.1/bin ]] && PATH=~/.gem/ruby/1.9.1/bin:$PATH
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

# chef

# TODO is there knife plugin stuff for this in oh-my-zsh?
alias knife="nocorrect knife"
alias k="nocorrect bundle exec knife"

kup() {
    knife cookbook upload --all
    for role in roles/*; do
        knife role from file $role
    done
    for bag in data_bags/*; do
        knife data bag create $bag
        for file in data_bags/$bag/*.{json,js,rb}; do
            knife data bag from file $bag $file
        done
    done
}

# vagrant

alias v="noglob bundle exec vagrant"

# s3cmd

alias s3cmd="nocorrect s3cmd"

# nix

[[ -f /usr/local/etc/profile.d/nix.sh ]] \
    && source /usr/local/etc/profile.d/nix.sh

############################################################################
# Provide higer-order functions
# http://yannesposito.com/Scratch/en/blog/Higher-order-function-in-zsh/
############################################################################

# usage:
#
# $ foo(){print "x: $1"}
# $ map foo a b c d
# x: a
# x: b
# x: c
# x: d
function map {
    local func_name=$1
    shift
    for elem in $@; print -- $(eval $func_name $elem)
}

# $ bar() { print $(($1 + $2)) }
# $ fold bar 0 1 2 3 4 5
# 15
# -- but also
# $ fold bar 0 $( seq 1 100 )
function fold {
    if (($#<2)) {
            print -- "ERROR fold use at least 2 arguments" >&2
            return 1
        }
        if (($#<3)) {
                print -- $2
                return 0
            } else {
                local acc
                local right
                local func_name=$1
                local init_value=$2
                local first_value=$3
                shift 3
                right=$( fold $func_name $init_value $@ )
                acc=$( eval "$func_name $first_value $right" )
                print -- $acc
                return 0
            }
}

# usage:
#
# $ baz() { print $1 | grep baz }
# $ filter baz titi bazaar biz
# bazaar
function filter {
    local predicate=$1
    local result
    typeset -a result
    shift
    for elem in $@; do
        if eval $predicate $elem >/dev/null; then
            result=( $result $elem )
        fi
    done
    print $result
}
