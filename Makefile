#! /usr/bin/make -f

all: all-native

bin-native	= fouine
bin-bytecode	= fouine.byte

all-native:
	ocamlbuild -lib unix -use-menhir -menhir ../menhir.sh src/main.native
	@ mv main.native $(bin-native)

all-bytecode:
	ocamlbuild src/main.byte
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
	ocamlbuild -r -clean

.PHONY: all all-native all-bytecode clean
