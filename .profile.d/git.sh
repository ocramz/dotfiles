#-*- mode: sh -*-

export GIT_EDITOR=nano

alias g=git

function git-merged-local-branches {
    for x in $(git b|grep -v master); do
        (git branch --contains $x|grep master>/dev/null) && echo "$x was already merged"
    done
}

function git-merged-remote-branches {
    for x in $(git b -a|grep origin|cut -d'/' -f2-5|grep -v master); do
        (git branch --contains $x|grep master>/dev/null) && echo "$x was already merged"
    done
}
