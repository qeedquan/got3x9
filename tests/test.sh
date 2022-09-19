#!/bin/sh

set -x
set -e

T3X9DIR="$(pwd)/../t3x9r4"
GOT3X9DIR="$(pwd)/.."
TESTDIR=$(pwd)

build_bin() {
	cd $GOT3X9DIR
	make
	cp bin/t9 $TESTDIR

	cd $TESTDIR
	cc -o tcvm $T3X9DIR/tcvm.c
}

test_vm() {
	for f in $T3X9DIR/*.t
	do
		p=$(basename $f)
		./tcvm $T3X9DIR/t.vm < $f > tc-$p.new
		./t9 $T3X9DIR/t.vm < $f > t9-$p.new
		diff tc-$p.new t9-$p.new
	done
}

test_programs() {
	for i in *.t
	do
		p=$(basename $i .t).out
		./tcvm $T3X9DIR/t.vm < $i > 1-$p
		./t9 -o 2-$p $i
		./t9 -d 1-$p > 1dis-$p
		./t9 -d 2-$p > 2dis-$p
		diff -u 1dis-$p 2dis-$p
	done
}

cleanup() {
	rm -f tcvm t9 *.new *.out *.vm
}

build_bin
test_vm
test_programs
cleanup
