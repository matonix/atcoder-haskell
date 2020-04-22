#!/bin/bash
if [[ "$1" =~ ^(a|b|c|d|e|f)$ ]]; then
    contest=${PWD##*/}
    # ghcid --command "stack ghci $contest:exe:$1" --test Test.main
    # use ghci instead of ghcie, because current ghcie cannot use hDuplicate.
    # see: https://github.com/ndmitchell/ghcid/issues/274
    if [[ "$2" =~ ^(ghci)$ ]]; then
        stack ghci $contest:exe:$1
    else
        stack ghci $contest:exe:$1 --ghci-options "-e Test.main"
    fi
else
    echo "\"$1\" is not in the list"
fi
