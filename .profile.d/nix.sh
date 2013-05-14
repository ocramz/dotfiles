#-*- mode: sh -*-

if [ -f ~/.nix-profile ]; then
    # export ACLOCAL_PATH=~/.nix-profile/share/aclocal:$ACLOCAL_PATH
    # export PKG_CONFIG_PATH=~/.nix-profile/lib/pkgconfig:$PKG_CONFIG_PATH
    # export C_INCLUDE_PATH=~/.nix-profile/include:$C_INCLUDE_PATH 
    # export LD_RUN_PATH=~/.nix-profile/lib:~/.nix-profile/lib64:$LD_RUN_PATH
    export MANPATH=~/.nix-profile/share/man:$MANPATH
    export NIX_GHC_LIBDIR=~/.nix-profile/lib
fi
