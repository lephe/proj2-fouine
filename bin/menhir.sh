#! /usr/bin/env bash

# Here's a simple trick to pass options to menhir without using myocamlbuild.ml
menhir --dump --explain "$@"
