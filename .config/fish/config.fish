# basics

alias l="ls -la"

# emacs

if test -f  -f /Applications/Emacs.app
  set PATH /Applications/Emacs.app/*/*/bin $PATH
end

set -g EDITOR "emacsclient -t --alternate-editor=''"

alias e=$EDITOR
alias ec="e -s"

function es
  emacs --daemon=$argv[1]; and ec $argv
end

function ek
  ec $argv -e '(server-stop)'
end

function el
  ps aux | egrep "[Ee]macs" | egrep -v grep
end

# tmux

alias t="tmux attach; or tmux"

function ts
  tmux -L $argv
end

function ta
  ts $argv attach
end

function tk
  ts $argv kill-session
end

function tl
  ps aux | grep tmux | egrep -v grep
end

# git

alias g=git

# haskell

if test -d ~/.cabal/bin; and status --is-login
  set PATH ~/.cabal/bin $PATH
end

alias c=cabal-dev

# erlang

alias r="rebar skip_deps=true"

function eunit
  while inotifywait -e modify -r $PWD/src $PWD/test $PWD/include
    r compile; and r eunit
  end
end

# ruby

if test -d ~/.rbenv/bin; and status --is-login
  set PATH ~/.rbenv/bin $PATH
end

# chef

alias k=knife

# vagrant

alias v=vagrant

# archlinux

if test -f /etc/arch-release
  set -g JAVA_HOME /usr/lib/jvm/java-7-openjdk
end

# mac

if test -d /Applications
  set -g JAVA_HOME `/usr/libexec/java_home`
end
