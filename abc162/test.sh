#!/bin/bash
if [[ "$1" =~ ^(a|b|c|d|e|f)$ ]]; then
    contest=${PWD##*/}
    # ghcid --command "stack ghci $contest:exe:$1" --test Test.main
    # use ghci instead of ghcid, because current ghcid cannot use hDuplicate.
    # see: https://github.com/ndmitchell/ghcid/issues/274
    if [[ "$2" =~ ^(ghci)$ ]]; then
        stack ghci $contest:exe:$1
    elif [[ "$2" =~ ^(time)$ ]]; then
        stack ghc -- -O2 -rtsopts $1/Main.hs
        for i in {1..3} ; do
        	if [[ -s $1/input$i ]]; then
                echo "input$i"
                $1/Main +RTS -t --machine-readable < $1/input$i 2>&1 > /dev/null | grep -e total_wall_seconds -e max_bytes_used
            fi
        done
    else
        stack ghci $contest:exe:$1 --ghci-options "-e Test.main"
    fi
else
    echo "\"$1\" is not in the list"
fi
