# QnD makefile. Not sure if we can use cabal here, too.
$(shell mkdir -p build)
ifndef HC
HC=ghc
endif
ifndef OPT
OPT=-O
endif
OPT += -hidir build -odir build
all: c_test round_trip render_tests report_fatal check_args
c_test:
	$(HC) $(OPT) -o bin/CTest -i../src -isrc --make src/CTest.hs
round_trip:
	$(HC) $(OPT) -o bin/CRoundTrip -i../src -isrc --make src/CRoundTrip.hs
render_tests:
	$(HC) $(OPT) -o bin/RenderTests -i../src -isrc --make src/RenderTests.hs
	rm build/Main.o # GHC bug: Main of report_fatal isn't recompiled (6.8.2, 6.8.3, should get fixed in 6.10)
report_fatal:
	$(HC) $(OPT) -o bin/ReportFatal -i../src -isrc --make src/ReportFatal.hs
check_args:
	$(HC) $(OPT) -o bin/CheckGccArgs -i../src -isrc --make src/CheckGccArgs.hs

clean:
	rm -rf build/*
	