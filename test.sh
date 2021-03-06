#! /usr/bin/env bash

# Parsing tests
tests_parsing="toplevel"
# Output tests
tests_output="calc let func rec ref tuple"
# Zero tests
tests_zero="adt list try"
# Exceptions tests
tests_except="except"

# Performed tests
performed=0
# Passed tests
passed=0

r="\e[31;1m"
g="\e[32;1m"
y="\e[33;1m"
b="\e[34;1m"
p="\e[35;1m"
w="\e[0m"
W="\e[0m\e[1m"
x="\e[34m"

prelude="tests/prelude.ml"

# Additional options to pass to fouine
fopt=""
# Enable syntax highlighting if program "highlight" is present
highlight=$(which highlight 2> /dev/null)
# Whether to pass Fouine's -debug output to OCaml
replicate=
# Transformations to test
transf=
# Whether to test the stack machine
machine=

# Parse command-line options
#   -replicate triggers testing fouine's -debug output through ocaml
#   -R, -E, -ER and -RE trigger testing the transformations
#   Other options go directly to fouine for debug
for arg; do case "$arg" in
	"-replicate")	replicate=true;;
	"-R")		transf="$transf -R";;
	"-E")		transf="$transf -E";;
	"-ER")		transf="$transf -ER";;
	"-RE")		transf="$transf -RE";;
	"-machine")	machine=true;;
	*)		fopt="$fopt $arg";;
esac; done

echo -e ""
echo -e "$x=-=-=$W Automated tests $w$x=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=$w"
echo -e ""
echo -e "  ${y}PARSING${w}    - Fouine should parse iff OCaml does"
echo -e "  ${b}OCAML${w}      - Compare Fouine output with that of OCaml"
echo -e "  ${p}ZERO${w}       - Fouine should succeed and print 0"
echo -e "  ${r}EXCEPTIONS${w} - Fouine should catch an exception and fail"
echo -e "  ${W}REPLICATE${w}  - Compare Fouine and Fouine -debug | OCaml"
echo -e "  ${W}TRANSF ...${w} - Compare original and transformed programs"
echo -e ""
echo -e "Additional Fouine options:${W}$fopt${w}"
echo -e ""
echo -e "$x=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=$w"
echo -e ""

function show_output()
{
	echo -ne "${r}$1 produced "
	if [[ "$2" ]]; then
		echo -e "the following output:${w}"
		echo "$2"
	else
		echo -e "no output${w}"
	fi
}

function show_input()
{
	echo -e "${r}On this input:${w}"
	if [[ $highlight ]]
	then $highlight -O xterm256 "$1"
	else cat "$1"
	fi
}

# Check that fouine output is the same as OCaml
function compare()
{
	out_ocaml=$(cat "$prelude" "$2" | ocaml -stdin)
	ret_ocaml="$?"

	out_fouine=$(./fouine $fopt "$1")
	ret_fouine="$?"

	if [[ $out_ocaml == $out_fouine && $ret_ocaml == 0
		&& $ret_fouine == 0 ]]; then
		echo -e "${g}●${w} $1"
		return 0
	fi

	echo -e "${r}●${w} $1"
	show_output "Ocaml" "$out_ocaml"
	show_output "Fouine" "$out_fouine"
	show_input "$1"
	echo ""

	return 1
}

# Check that the source parses in fouine as in OCaml
function parse()
{
	ocaml "$1" 1> /dev/null 2>&1
	# Check only the return status
	ret_ocaml="$?"
	[[ $ret_ocaml != 0 ]] && ret_ocaml=1

	./fouine -parse $fopt "$1" 1> /dev/null 2>&1
	# Same here, only the return status matters
	ret_fouine="$?"
	[[ $ret_fouine != 0 ]] && ret_fouine=1

	if [[ $ret_ocaml == $ret_fouine ]]; then
		echo -e "${g}●${w} $ret_ocaml $1"
		return 0
	fi

	echo -e "${r}●${w} $1"
	echo "Ocaml returned $ret_ocaml but fouine returned $ret_fouine."
	show_input "$1"
	echo ""

	return 1
}

# Check that output is zero (when code cannot be executed by OCaml
function zero()
{
	out=$(./fouine $fopt "$1")
	ret="$?"

	if [[ $ret != 0 ]]; then
		echo -e "${r}●${w} $1"
		echo "An exception occurred!"
		show_input "$1"
		return 1
	fi

	if [[ $out != 0 ]]; then
		echo -e "${r}●${w} $1"
		echo "Fouine did not print 0."
		show_input "$1"
		return 1
	fi

	echo -e "${g}●${w} $1"
	return 0
}

# Check that exceptions are detected and reported
function except()
{
	# Check that the file parses, but fails *at runtime*
	./fouine -parse $fopt "$1" >/dev/null 2>&1
	ret="$?"

	if [[ $ret != 0 ]]; then
		echo -e "${r}●${w} $1"
		echo "Failed during parsing."
		show_input "$1"
		return 1
	fi

	stderr=$( (./fouine $fopt "$1" 3>&2 2>&1 1>&3-) 2> /dev/null )
	ret="$?"

	# Check that the return status is nonzero AND stderr is not empty
	if [[ $ret != 0 && $stderr ]]; then
		echo -e "${g}●${w} $1"
		return 0
	fi

	echo -e "${r}●${w} $1"
	[[ $ret == 0 ]] && echo "Fouine returned 0."
	[[ ! $stderr ]] && echo "Fouine did not emit a diagnostic on stderr."
	show_input "$1"
	echo ""

	return 1
}

# Check that Fouine and OCaml on Fouine's debug return the same thing
function replicate()
{
	./fouine -parse $fopt "$1" -debug 2> /dev/null > /tmp/fouine.ml

	if [[ $? != 0 ]]; then
		echo -e "${r}●${w} $1"
		echo "Fouine did not return 0."
		return 1
	fi

	compare "$1" "/tmp/fouine.ml"
}

# Test transformations
function transform()
{
	out_transf=$(./fouine $fopt $1 "$2")
	ret_transf="$?"

	out_fouine=$(./fouine $fopt "$2")
	ret_fouine="$?"

	if [[ $out_transf == $out_fouine && $ret_transf == 0
		&& $ret_fouine == 0 ]]; then
		echo -e "${g}●${w} $2"
		return 0
	fi

	echo -e "${r}●${w} $2"
	show_output "Transformed program" "$out_transf"
	show_output "Fouine" "$out_fouine"
	show_input "$2"
	echo ""

	return 1
}

# Parsing tests
for folder in $tests_parsing; do
	echo -e "${y}PARSING: $folder${w}"
	for file in tests/$folder/*; do
		parse "$file"
		passed=$(($passed + 1 - $?))
		performed=$(($performed + 1))
	done
	echo ""
done

# Output tests
for folder in $tests_output; do
	echo -e "${b}OCAML: $folder${w}"
	for file in tests/$folder/*; do
		compare "$file" "$file"
		passed=$(($passed + 1 - $?))
		performed=$(($performed + 1))
	done
	echo ""
done

# Zero tests
for folder in $tests_zero; do
	echo -e "${p}ZERO: $folder${w}"
	for file in tests/$folder/*; do
		zero "$file"
		passed=$(($passed + 1 - $?))
		performed=$(($performed + 1))
	done
	echo ""
done

# Exception tests
for folder in $tests_except; do
	echo -e "${r}EXCEPTIONS: $folder${w}"
	for file in tests/$folder/*; do
		except "$file"
		passed=$(($passed + 1 - $?))
		performed=$(($performed + 1))
	done
	echo ""
done

# Replication tests, if -replicate is on
if [[ $replicate ]]; then
	for folder in $tests_output; do
		echo -e "${W}REPLICATE: $folder${w}"
		for file in tests/$folder/*; do
			replicate "$file"
			passed=$(($passed + 1 - $?))
			performed=$(($performed + 1))
		done
		echo ""
	done
fi

# Transformation tests
for folder in $tests_output $tests_zero; do
	for tr in $transf; do
		echo -e "${W}TRANSFORM $tr: $folder${w}"
		for file in tests/$folder/*; do
			transform "$tr" "$file"
			passed=$(($passed + 1 - $?))
			performed=$(($performed + 1))
		done
		echo ""
	done

	if [[ $machine ]]; then
		echo -e "${W}MACHINE: $folder${w}"
		for file in tests/$folder/*; do
			transform "-machine" "$file"
			passed=$(($passed + 1 - $?))
			performed=$(($performed + 1))
		done
		echo ""
	fi
done

# Okay!
echo "Done! Passed $passed/$performed tests."
[[ $passed != $performed ]] && exit 1 || exit 0
