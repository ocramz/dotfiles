#-*- mode: sh -*-

alias t='tmux -2 attach || tmux -2'

function ts {
    tmux -S /tmp/$@
}

function ta {
    ts $1 attach
}

function tk {
    ts $1 kill-session
}

function tl {
    ps aux|grep tmux|egrep -v grep
}
