# QnD makefile. 
# It is possible to use cabal here ? 
# We _do not want_ to depend on an installed version of language.c
# Note that you have to make sure Lexer.hs and Parser.hs is available and 
# up-to-date in /src/Language/C/Parser/

$(shell mkdir -p build/autogen/Language/C/Parser build_p)
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
INCLUDE=-ibuild/autogen -i../src -isrc
# GHC bug: Main sometimes isn't recompiled (6.8.2, 6.8.3, should get fixed in 6.10)
# Therefore we add rm build/Main.o to the end of make targets
c_test: autogen
	$(HC) $(OPT) -o bin/CTest$(SUFFIX) $(INCLUDE) --make src/CTest.hs
	rm build/Main.o 
parse: autogen
	$(HC) $(OPT) -o bin/CParse$(SUFFIX) $(INCLUDE) --make src/CParse.hs
	rm build/Main.o 
round_trip: autogen
	$(HC) $(OPT) -o bin/CRoundTrip$(SUFFIX) $(INCLUDE) --make src/CRoundTrip.hs
	rm build/Main.o 
equiv: autogen
	$(HC) $(OPT) -o bin/CEquiv$(SUFFIX) $(INCLUDE) --make src/CEquiv.hs
	rm build/Main.o 
render_tests: autogen
	$(HC) $(OPT) -o bin/RenderTests$(SUFFIX) $(INCLUDE) --make src/RenderTests.hs
	rm build/Main.o 
report_fatal: autogen
	$(HC) $(OPT) -o bin/ReportFatal$(SUFFIX) $(INCLUDE) --make src/ReportFatal.hs
	rm build/Main.o 
check_args: autogen
	$(HC) $(OPT) -o bin/CheckGccArgs$(SUFFIX) $(INCLUDE) --make src/CheckGccArgs.hs
	rm build/Main.o 
clean:
	rm -rf build/*
	
autogen: build/autogen/Language/C/Parser/Lexer.hs build/autogen/Language/C/Parser/Parser.hs
build/autogen/Language/C/Parser/Lexer.hs: ../src/Language/C/Parser/Lexer.x
	alex -g -o $@ $<
build/autogen/Language/C/Parser/Parser.hs: ../src/Language/C/Parser/Parser.y
	happy -gca -o $@ $<