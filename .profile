#-*- mode: sh -*-

if [ -d /etc/profile.d ]; then
    for x in /etc/profile.d/*.sh; do
        . $x
    done
fi

if [ -d ~/.profile.d ]; then
    for x in ~/.profile.d/*.sh; do
        . $x
    done
fi
