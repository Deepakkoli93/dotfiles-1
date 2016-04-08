#!/usr/bin/env bash
## Open the current file in emacs and change focus to the window.
## I assume that I started emacs with --name 'MyEmacs'. This makes
## it easy for me to refer to the emacs window. 

if [[ $# -ne 3 ]]; then
    echo "Usage: $0 <line> <column> <file>"
    exit 1
fi

line=$1
column=$2
file=$3

emacsclient +${line}:${column} -n ${file}
wmctrl -a MyEmacs

