#-*- mode: sh -*-

# DEFAULTS
[ -d /etc/profile.d ] && for i in /etc/profile.d/*.sh; do
    . $i
done

# NIX
export C_INCLUDE_PATH=\
~/.nix-profile/include:\
/usr/include:\
/usr/local/include
export LD_RUN_PATH=\
~/.nix-profile/lib:~/.nix-profile/lib64:\
/lib:/lib64:/lib/x86_64-linux-gnu:\
/usr/lib:/usr/lib/x86_64-linux-gnu:\
/usr/local/lib

# EMACS
export EDITOR='emacs -nw'

# GIT
export GIT_EDITOR=nano

# HASKELL
PATH=~/.cabal/bin:$PATH

# JAVA
if [ -f /mach_kernel ]; then # MAC
    export JAVA_HOME="$(/usr/libexec/java_home)"
fi
export MAVEN_OPTS="-Xmx512m -XX:MaxPermSize=128m"

# PYTHON
PATH=~/.local/bin:$PATH
export WORKON_HOME=$HOME/.virtualenvs
[ -f ~/.local/bin/virtualenvwrapper.sh ] && . ~/.local/bin/virtualenvwrapper.sh

# RUBY
which rbenv >/dev/null && eval "$(rbenv init -)"

# GO
# export GOROOT=~/go
# PATH=$GOROOT/bin:$PATH

# OCAML
# which opam >/dev/null && eval "$(opam config -env)"

# ~
PATH=~/bin:$PATH
