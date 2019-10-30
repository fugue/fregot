#!/bin/bash
set -o nounset -o errexit -o pipefail

input1='watch-input-1.json'
input2='watch-input-2.json'
pipe="$(mktemp -u)"

trap "rm -f \"$pipe\" \"$input1\" \"$input2\"" EXIT

echo '{"name": "input-1"}' >"$input1"
echo '{"name": "input-2"}' >"$input2"
mkfifo "$pipe"

sleep 500 >"$pipe" 2>/dev/null &  # Just to keep this file open.
doorstop_pid=$!

fregot repl --watch <"$pipe" &
fregot_pid="$!"

function enter() {
    echo "$1" >>"$pipe"
    sleep 0.1
}

enter ":input $input1"
enter "input"
enter ":input $input2"
enter "input"
echo '{"name": "input-changed!"}' >"$input2"
sleep 0.1
enter "input"

kill "$doorstop_pid" >/dev/null 2>&1 || true
kill "$fregot_pid"   >/dev/null 2>&1 || true
