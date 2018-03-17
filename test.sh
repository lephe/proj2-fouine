#! /usr/bin/env bash

# Parsing tests
tests_parsing="toplevel"
# Output tests
tests_output="calc func let"

# Performed tests
performed=0
# Passed tests
passed=0

r="\e[31;1m"
g="\e[32;1m"
b="\e[0m\e[35;1m"
w="\e[0m"

prelude="tests/prelude.ml"

# Additional options to pass to fouine
fopt="$@"
# Enable syntax highlighting if program "highlight" is present
highlight=$(which highlight 2> /dev/null)

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

function compare()
{
	out_ocaml=$(cat "$prelude" "$1" | ocaml -stdin)
	out_fouine=$(bin/fouine "$fopt" "$1")

	if [[ $out_ocaml == $out_fouine ]]; then
		echo -e "${g}●${w} $1"
		return 1
	fi

	echo -e "${r}●${w} $1"
	show_output "Ocaml" "$out_ocaml"
	show_output "Fouine" "$out_fouine"
	show_input "$1"
	echo ""

	return 0
}

function parse()
{
	cat "$prelude" "$1" | ocaml -stdin 1> /dev/null 2>&1
	# Check only the return status
	ret_ocaml="$?"
	[[ $ret_ocaml != 0 ]] && ret_ocaml=1

	bin/fouine -parse "$fopt" "$1" 1> /dev/null 2>&1
	# Same here, only the return status matters
	ret_fouine="$?"
	[[ $ret_fouine != 0 ]] && ret_fouine=1

	if [[ $ret_ocaml == $ret_fouine ]]; then
		echo -e "${g}●${w} $ret_ocaml $1"
		return 1
	fi

	echo -e "${r}●${w} $1"
	echo "Ocaml returned $ret_ocaml but fouine returned $ret_fouine."
	show_input "$1"
	echo ""

	return 0
}

# Parsing tests
echo -e "\n${b}PARSING TESTS${w} - check the return status of the parser\n"

for folder in $tests_parsing; do
	for file in tests/$folder/*; do
		parse "$file"
		performed=$(($performed + 1))
		passed=$(($passed + 1 - $?))
	done
done

# Output tests
echo -e "\n${b}OUTPUT TESTS${w} - compare the results of execution\n"

for folder in $tests_output; do
	for file in tests/$folder/*; do
		compare "$file"
		performed=$(($performed + 1))
		passed=$(($passed + 1 - $?))
	done
	echo ""
done

# Okay!
[[ $passed != $performed ]] && exit 1 || exit 0
# echo -e "\nDone! Passed $passed/$performed tests."
