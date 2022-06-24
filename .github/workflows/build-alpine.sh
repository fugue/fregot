#!/bin/sh
set -e

apk add --no-cache curl gcc g++ gmp-dev ncurses-dev libffi-dev make xz gzip tar perl git bash sudo binutils-gold
curl --proto '=https' --tlsv1.2 -sSf https://downloads.haskell.org/~ghcup/0.1.17.8/x86_64-linux-ghcup-0.1.17.8 >/usr/bin/ghcup

chmod +x /usr/bin/ghcup
export PATH="$HOME/.ghcup/bin:$PATH"
ghcup install ghc 9.0.2
ghcup set ghc 9.0.2
ghcup install stack 2.7.5

apk add zlib-dev zlib-static ncurses-static
cp  /usr/lib/gcc/x86_64-alpine-linux-musl/11.2.1/crtbeginS.o \
    /usr/lib/gcc/x86_64-alpine-linux-musl/11.2.1/crtbeginT.o
cp  /usr/lib/gcc/x86_64-alpine-linux-musl/11.2.1/crtendS.o \
    /usr/lib/gcc/x86_64-alpine-linux-musl/11.2.1/crtend.o
stack build --allow-different-user --system-ghc --copy-bins --ghc-options='-optl-static'
cp $HOME/.local/bin/fregot fregot
