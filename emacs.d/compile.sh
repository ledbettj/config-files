#!/usr/bin/env bash

find ./site-lisp/ -name "*.el" -print0 | xargs -0 emacs -batch -f batch-byte-compile
