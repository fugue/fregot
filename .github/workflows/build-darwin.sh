#!/usr/bin/env bash
set -e errexit

url -sSL https://get.haskellstack.org/ | sh

stack build --copy-bins
cp $HOME/.local/bin/fregot fregot
