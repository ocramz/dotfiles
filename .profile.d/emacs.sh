#-*- mode: sh -*-

export EDITOR='emacs -nw'

alias emacs='emacs -nw'
alias e=emacs

function ec {
    e -s $@
}

function es {
    emacs --daemon=$1 && ec $@
}

function ek {
    ec $1 -e '(server-stop)'
}

function el {
    ps aux|egrep '[Ee]macs'|egrep -v grep
}
