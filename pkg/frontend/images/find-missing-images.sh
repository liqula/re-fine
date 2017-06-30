#!/bin/bash

set -e

# print all jokers.
( for mode in `echo bright dark RO`; do \
    for file in `ls *_$mode.svg`; do \
        cmp -s $file 00_joker_$mode.svg && echo $file ; \
    done ; \
done ) | sort
