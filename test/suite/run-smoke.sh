#!/bin/sh
source ./configuration

source $CTEST_BINDIR/setup_test_suite smoke

# export CTEST_DEBUG=1

cd smoke

export CTEST_DRIVER=CParse
sh run-test doesnotexist.c
export CTEST_NON_PARSE=1
sh run-test test_non_parse.c
export CTEST_NON_PARSE=0
sh run-test test.c

export CTEST_DRIVER=CRoundTrip
for f in `ls *.c | grep -v non_parse | grep -v equiv`; do sh run-test $f; done;

export CTEST_DRIVER=CEquiv
export CTEST_NON_EQUIV=1
sh run-test test.c test1.c
sh run-test test_attr.non_equiv_1.c test_attr.non_equiv_2.c
unset CTEST_NON_EQUIV
sh run-test test.c test.c
