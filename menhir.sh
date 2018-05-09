#! /usr/bin/env bash

# I did try to use a _tags file to do the preprocessing, but apparently the
# Menhir rule overrides pp().

# First pass the source through m4...
m4 src/parser.mly > parser.mly

# Then rewrite the arguments and invoke Menhir with a few custom options
# The basename here is required because the modified m4 file is in _build but
# ocamlbuild expects the output file in _build/src.
menhir --dump --explain -b "src/parser" "${@/src\/parser.mly/parser.mly}"
