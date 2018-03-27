#! /usr/bin/make -f

all: all-native

bin-native	= bin/fouine
bin-bytecode	= bin/fouine.byte

all-native: | bin/
	ocamlbuild -lib unix -use-menhir -menhir ../bin/menhir.sh src/main.native
	@ mv main.native $(bin-native)

all-bytecode: | bin/
	ocamlbuild -I src src/main.byte
	@ mv main.byte $(bin-bytecode)

test-all: all-native
	@ ./test.sh -bootstrap
test: all-native
	@ ./test.sh
test-ast: all-native
	@ ./test.sh -ast

%/:
	@mkdir -p $@

clean:
	ocamlbuild -clean

.PHONY: all all-native all-bytecode clean
