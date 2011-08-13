# Inplace Testing
#
# TODO: This is a QnD Makefile.
# It is especially annoying that we have to rebuild the executables every time
# HAPPY_OPTS=-scg
HAPPY_OPTS=-sacg
ifndef HC
	HC=ghc
endif

ifndef OPT
	OPT=-O -rtsopts
endif

ifdef PROFILE
	OPT += -prof -auto-all
	SUFFIX=_p
endif

BUILD_DIR=build$(SUFFIX)

OPT += -hidir $(BUILD_DIR) -odir $(BUILD_DIR)

INCLUDE=-ibuild/autogen -i../src -i../test-framework -isrc

all: c_test test_drivers check_args render_tests

profile : OPT += -prof -auto-all
profile : SUFFIX=_p
profile : test_drivers c_test

test_drivers: parse round_trip equiv report_fatal

# GHC bug: Main sometimes isn't recompiled (6.8.2, 6.8.3, should get fixed in 6.10)
# Therefore we add rm build/Main.o to the end of make targets
c_test: autogen
	$(HC) $(OPT) -o bin/CTest$(SUFFIX) $(INCLUDE) --make src/CTest.hs
	rm $(BUILD_DIR)/Main.o 
parse: autogen
	$(HC) $(OPT) -o bin/CParse$(SUFFIX) $(INCLUDE) --make src/CParse.hs
	rm $(BUILD_DIR)/Main.o 
round_trip: autogen
	$(HC) $(OPT) -o bin/CRoundTrip$(SUFFIX) $(INCLUDE) --make src/CRoundTrip.hs
	rm $(BUILD_DIR)/Main.o 
equiv: autogen
	$(HC) $(OPT) -o bin/CEquiv$(SUFFIX) $(INCLUDE) --make src/CEquiv.hs
	rm $(BUILD_DIR)/Main.o 
render_tests: autogen
	$(HC) $(OPT) -o bin/RenderTests$(SUFFIX) $(INCLUDE) --make src/RenderTests.hs
	rm $(BUILD_DIR)/Main.o 
report_fatal: autogen
	$(HC) $(OPT) -o bin/ReportFatal$(SUFFIX) $(INCLUDE) --make src/ReportFatal.hs
	rm $(BUILD_DIR)/Main.o 
check_args: autogen
	$(HC) $(OPT) -o bin/CheckGccArgs$(SUFFIX) $(INCLUDE) --make src/CheckGccArgs.hs
	rm build/Main.o 
clean:
	rm -rf build/*

autogen: build/autogen/Language/C/Parser/Lexer.hs build/autogen/Language/C/Parser/Parser.hs
build/autogen/Language/C/Parser/Lexer.hs: ../src/Language/C/Parser/Lexer.x
	mkdir -p build/autogen/Language/C/Parser
	alex -g -o $@ $<
build/autogen/Language/C/Parser/Parser.hs: ../src/Language/C/Parser/Parser.y
	mkdir -p build/autogen/Language/C/Parser
	happy $(HAPPY_OPTS) -o $@ $<