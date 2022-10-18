#!/usr/bin/env nix-shell
#! nix-shell -i bash -p entr
# shellcheck shell=bash

git ls-files | entr -cc -s "nim r src/main.nim"
