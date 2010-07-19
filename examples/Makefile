VERSION?=$(shell cat ../language-c.cabal | grep '^Version:' | sed  -E 's/[ \t]+/ /g' | cut -sd' ' -f2)

all:
	ghc -package language-c-$(VERSION) --make -O ParseAndPrint.hs
	ghc -package language-c-$(VERSION) --make -O ScanFile.hs
	ghc -package language-c-$(VERSION) --make -O SearchDef.hs
	ghc -package language-c-$(VERSION) --make -O TypeCheck.hs
	ghc -package language-c-$(VERSION) --make -O ComputeSize.hs
clean:
	rm -f *.o *.hi C2HSTest ComputeSize QuickTest ScanFile SearchDef TypeCheck ParseAndPrint
clean_tmp:
	rm -f *.o *.hi
