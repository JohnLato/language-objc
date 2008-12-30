all:
	ghc -package language-c --make -O ComputeSize.hs
	ghc -package language-c --make -O ScanFile.hs
	ghc -package language-c --make -O SearchDef.hs
	ghc -package language-c --make -O TypeCheck.hs
clean:
	rm -f *.o *.hi C2HSTest ComputeSize QuickTest ScanFile SearchDef TypeCheck
clean_tmp:
	rm -f *.o *.hi
