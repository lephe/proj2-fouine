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
	@ ./test.sh -replicate -E -R -RE -ER -machine
test-machine: all-native
	@ ./test.sh -machine
test-typing: all-native
	@ ./test.sh -typing
test: all-native
	@ ./test.sh

%/:
	@mkdir -p $@

clean:
	ocamlbuild -r -clean

.PHONY: all all-native all-bytecode clean
