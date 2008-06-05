#!/bin/sh
source ./configuration

export TESTNAME=smoke
export CTEST_DEBUG=1
export CTEST_DRIVER=CRoundTrip

source $CTEST_BINDIR/setup_test_suite || { echo "Setup failed" 1>&2 ; exit 1 ; }

cd smoke
run-test doesnotexist.c
for f in `ls *.c`;  do 
	run-test $f
done
