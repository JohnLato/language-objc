#!/bin/sh
source ./configuration

TEST_SUITES="gcc-dg-incompliant gcc-dg-c89 gcc-dg-c99 gcc-dg-gnu99"
for t in $TEST_SUITES; do
	echo "Cleaning test suite $t"
	sh clear_test_suite $t
done

BASE_DIR=`pwd`
cd gcc.dg
DG_DIR=`pwd`

for cf in `find . -name '*.c'`; do
	cd $DG_DIR/`dirname $cf`
	f=`basename $cf`

	grep -e "^$f" $BASE_DIR/dg-ignore.txt
	if [ $? -eq 0 ]; then echo " ... skipped"; continue; fi

	COMPLIANCE=
	gcc -fsyntax-only -ansi -pedantic-errors  $f 2>/dev/null
	if [ $? -eq 0 ] ; then COMPLIANCE=c89; fi
	if [ -z $COMPLIANCE ] ; then
		gcc -fsyntax-only -std=c99 -pedantic-errors  $f 2>/dev/null
		if [ $? -eq 0 ] ; then COMPLIANCE=c99; fi
	fi
	if [ -z $COMPLIANCE ] ; then
		gcc -fsyntax-only -std=gnu9x -pedantic-errors  $f 2>/dev/null
		if [ $? -eq 0 ] ; then COMPLIANCE=gnu99; fi
	fi
	if [ -z $COMPLIANCE ] ; then
		gcc -fsyntax-only -std=gnu9x $f 2>/dev/null
		if [ $? -eq 0 ] ; then COMPLIANCE=incompliant; fi
	fi
	if [ ! -z $COMPLIANCE ] ; then
		echo "[INFO] Running Test $f ($COMPLIANCE)"
		source $CTEST_BINDIR/set_test_suite gcc-dg-$COMPLIANCE
		export CTEST_DRIVER=CRoundTrip
		sh run-test $f
	fi
done
