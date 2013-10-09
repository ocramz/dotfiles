#-*- mode: sh -*-

if [ -e /etc/profile.d ]; then
    for i in /etc/profile.d/*.sh; do
        if [ -r $i ]; then
            . $i
        fi
    done
    unset i
fi

if [ -e $HOME/.profile.d ]; then
    for i in $HOME/.profile.d/*.sh; do
        if [ -r $i ]; then
            . $i
        fi
    done
    unset i
fi

if [ -e "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi
