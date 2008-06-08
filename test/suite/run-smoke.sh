#!/bin/sh
source ./configuration

source $CTEST_BINDIR/setup_test_suite smoke

export CTEST_DEBUG=1
export CTEST_DRIVER=CRoundTrip

cd smoke
run-test doesnotexist.c
for f in `ls *.c`;  do 
	sh run-test $f
done
