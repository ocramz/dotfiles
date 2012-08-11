# -*- mode: sh -*-

xset +dpms

setxkbmap \
    -layout dvorak \
    -option ctrl:nocaps,terminate:ctrl_alt_bksp,compose:ralt

exec gnome-session --session=xmonad
