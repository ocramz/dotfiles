#-*- mode: sh -*-

# DEFAULTS
[ -d /etc/profile.d ] && for i in /etc/profile.d/*.sh; do
    . $i
done

# NIX
export LD_RUN_PATH=~/.nix-profile/lib:~/.nix-profile/lib64

# EMACS
export EDITOR='emacs -nw'

# GIT
export GIT_EDITOR=nano

# HASKELL
PATH=~/.cabal/bin:$PATH

# JAVA
export MAVEN_OPTS="-Xmx512m -XX:MaxPermSize=128m"

# PYTHON
PATH=~/.local/bin:$PATH
export WORKON_HOME=/opt/virtualenvs ;# WORKON_HOME=$HOME/.virtualenvs
[ -f ~/.local/bin/virtualenvwrapper.sh ] && . ~/.local/bin/virtualenvwrapper.sh

# GO
export GOROOT=/usr/local/go

# OCAML
which opam >/dev/null && eval `opam config -env`

# ~
PATH=~/bin:$PATH
