#-*- mode: sh -*-

[ -d ~/.cabal/bin ] && PATH=~/.cabal/bin:$PATH

alias cabal='nocorrect cabal'
alias c=cabal-dev
