#-*- mode: sh -*-

# OH-MY-ZSH
if [ ! -d ~/.oh-my-zsh ]; then
    git clone http://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
fi

[[ -d ~/.oh-my-zsh ]] \
    && export ZSH=~/.oh-my-zsh \
    && plugins=(git rbenv bundler virtualenvwrapper) \
    source $ZSH/oh-my-zsh.sh

# TMUX
alias t='tmux -2 attach || tmux -2'
function ts() { tmux -S /tmp/$@ }
function ta() { ts $1 attach }
function tk() { ts $1 kill-session }
function tl() { ps aux|grep tmux|egrep -v grep }

# EMACS
alias emacs='emacs -nw'
alias e=emacs
function ec() { e -s $@ }
function es() { emacs --daemon=$1 && ec $@ }
function ek() { ec $1 -e '(server-stop)' }
function el() { ps aux|egrep '[Ee]macs'|egrep -v grep }

# GIT
alias git='nocorrect git'
alias g=git

# HASKELL
alias cabal='nocorrect cabal'
alias c=cabal-dev

# ERLANG
alias rebar='nocorrect rebar'
alias r='rebar skip_deps=true'

# RUBY
alias bundle='nocorrect bundle'
alias b=bundle

# CHEF
alias knife='nocorrect knife'
alias k=knife
chef-search() {
    knife exec -E "print nodes.find('$1').collect {|n| n.ec2.public_hostname}.join ' '"
}
chef-ssh() {
    query=$1; shift; pssh -l tim -H "$(chef-search $query)" $@
}

# VAGRANT
alias v=vagrant

# EACH
each() {
    find=$1 ; shift
    for found in $(find $PWD -name $find); do
        dir=$(dirname $found)
        pushd "$dir" >/dev/null
        echo $dir && $@
        popd >/dev/null
    done
}
