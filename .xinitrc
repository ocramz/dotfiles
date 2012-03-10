#!/bin/sh

xset +dpms

setxkbmap \
    -layout dvorak \
    -option ctrl:nocaps,terminate:ctrl_alt_bksp,compose:ralt

exec xfce4-session
