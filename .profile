#-*- mode: sh -*-

# DEFAULTS
[ -d /etc/profile.d ] && for i in /etc/profile.d/*.sh; do
    . $i
done

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
export VIRTUALENV_DISTRIBUTE=true
if [ -f /mach_kernel ]; then # MAC
    export PYTHONPATH=~/Library/Python/2.7/site-packages
    cat >~/.pydistutils.cfg <<\EOF
[install]
install_lib = ~/Library/Python/$py_version_short/site-packages
install_scripts = ~/bin
EOF
else
    export PYTHONPATH=~/lib/python/2.7/site-packages
    cat >~/.pydistutils.cfg <<\EOF
[install]
install_lib = ~/lib/python/$py_version_short/site-packages
install_scripts = ~/bin
EOF
fi

# GO
export GOROOT=/usr/local/go

# OCAML
which opam >/dev/null && eval `opam config -env`

# ~
PATH=~/bin:$PATH
