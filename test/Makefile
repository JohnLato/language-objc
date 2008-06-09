# QnD makefile. 
# It is possible to use cabal here ? 
# We _do not want_ to depend on an installed version of language.c
# Note that you have to make sure Lexer.hs and Parser.hs is available and 
# up-to-date in /src/Language/C/Parser/

$(shell mkdir -p build build_p)
ifndef HC
	HC=ghc
endif
ifndef OPT
	OPT=-O
endif
ifdef PROFILE
	OPT += -prof -auto-all
	SUFFIX=_p
endif
BUILD_DIR=build$(SUFFIX)
OPT += -hidir $(BUILD_DIR) -odir $(BUILD_DIR)
all: c_test test_drivers check_args render_tests
test_drivers: parse round_trip equiv report_fatal

# GHC bug: Main sometimes isn't recompiled (6.8.2, 6.8.3, should get fixed in 6.10)
# Therefore we add rm build/Main.o to the end of make targets
c_test:
	$(HC) $(OPT) -o bin/CTest$(SUFFIX) -i../src -isrc --make src/CTest.hs
	rm build/Main.o 
parse:
	$(HC) $(OPT) -o bin/CParse$(SUFFIX) -i../src -isrc --make src/CParse.hs
	rm build/Main.o 
round_trip:
	$(HC) $(OPT) -o bin/CRoundTrip$(SUFFIX) -i../src -isrc --make src/CRoundTrip.hs
	rm build/Main.o 
equiv:
	$(HC) $(OPT) -o bin/CEquiv$(SUFFIX) -i../src -isrc --make src/CEquiv.hs
	rm build/Main.o 
render_tests:
	$(HC) $(OPT) -o bin/RenderTests$(SUFFIX) -i../src -isrc --make src/RenderTests.hs
	rm build/Main.o 
report_fatal:
	$(HC) $(OPT) -o bin/ReportFatal$(SUFFIX) -i../src -isrc --make src/ReportFatal.hs
	rm build/Main.o 
check_args:
	$(HC) $(OPT) -o bin/CheckGccArgs$(SUFFIX) -i../src -isrc --make src/CheckGccArgs.hs
	rm build/Main.o 
clean:
	rm -rf build/*
	