#!/bin/bash

set -e
cd `dirname $0`/..

export RUN_ACCEPT_MARKER="___RUN_ACCEPT___"

case "$1" in
    tell)
        git status -s | grep -q '^[^ ]' || ( echo "$0: do not run this on dirty staging area."; exit 1 )
        git branch | head -1 | grep -q 'On branch master' && ( echo "$0: do not run this on master."; exit 1 )
        git commit --allow-empty -m "$RUN_ACCEPT_MARKER"
        git push -v
        ;;
    ask)
        git log --oneline | head -1 | grep -q "$RUN_ACCEPT_MARKER"
        ;;
    *)
        echo "$0: bad arguments."; exit 1
        ;;
esac
