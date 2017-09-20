#!/bin/bash

set -e

# print all jokers.
( for file in `ls icon/*.svg`; do \
    cmp -s $file 00_joker.svg && echo $file ; \
  done ) | sort
