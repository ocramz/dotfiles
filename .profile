#-*- mode: sh -*-

if [ -d /etc/profile.d ]; then
    for i in /etc/profile.d/*.sh; do
        . $i
    done
fi
