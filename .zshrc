#-*- mode: sh -*-

# OH-MY-ZSH
if [ ! -d ${HOME}/.oh-my-zsh ]; then
    git clone http://github.com/robbyrussell/oh-my-zsh.git ${HOME}/.oh-my-zsh
fi

[[ -d ~/.oh-my-zsh ]] \
    && export ZSH=~/.oh-my-zsh \
    && plugins=(git) source $ZSH/oh-my-zsh.sh

# BASICS
alias l="ls -la"

each() {
    find=${1} ; shift
    for found in $(find ${PWD} -name ${find}); do
        dir=$(dirname ${found})
        pushd "${dir}" >/dev/null
        echo ${dir} && ${@}
        popd >/dev/null
    done
}

# TMUX
alias t="tmux -2 attach || tmux -2"
function ts() { tmux -S /tmp/${@} }
function ta() { ts ${1 }attach }
function tk() { ts ${1} kill-session }
function tl() { ps aux | grep tmux | egrep -v grep }

# DVTM
alias d='dvtm -m ^z'

# EMACS
alias emacs=${EDITOR}
alias e=${EDITOR}
function ec() { e -s ${@} }
function es() { emacs --daemon=${1} && ec ${@} }
function ek() { ec ${1} -e '(server-stop)' }
function el() { ps aux | egrep "[Ee]macs" | egrep -v grep }

# JENKINS
alias jenkins='java -jar /usr/local/Library/LinkedKegs/jenkins/libexec/jenkins.war'

# GIT
alias git="nocorrect git"
alias g=git

# SCREENCASTING
screencast() {
    cvlc screen:// \
        --screen-fps=12 \
        --input-slave=alsa:// \
        --sout "\
#transcode{venc=x264,vcodec=h264,fps=12,vb=640,acodec=mp3,channels=1,ab=64}\
:std{access=file,mux=mp4,dst=screencast-2012-03-22-1332407228.avi}\
"
}

# HASKELL
alias c=cabal-dev

# ERLANG
alias rebar="nocorrect rebar"
alias r="rebar skip_deps=true"
export ERL_LIBS=${HOME}/src/proper
eunit() {
    while inotifywait -e modify -r ${PWD}/src ${PWD}/test ${PWD}/include; do
        echo "\n\nRun: $(date)"
        r compile && r eunit
        echo "End: $(date)\n\n"
    done
}

# CHEF
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

# VAGRANT
alias v="noglob bundle exec vagrant"

# S3CMD
alias s3="nocorrect s3cmd"

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
