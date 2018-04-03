#! /usr/bin/env bash

# First pass the source through m4...
m4 src/parser.mly > parser.mly

# Then rewrite the arguments and invoke Menhir with a few custom options
# The basename is here because the modified m4 file is in _build but ocamlbuild
# expects the output file in _build/src
menhir --dump --explain -b "src/parser" "${@/src\/parser.mly/parser.mly}"
