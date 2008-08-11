all:
	ghc -package language-c --make -O ComputeSize.hs
	ghc -package language-c --make -O ScanFile.hs
	ghc -package language-c --make -O SearchDef.hs
clean:
	rm -f *.o *.hi C2HSTest ComputeSize QuickTest ScanFile SearchDef
clean_tmp:
	rm -f *.o *.hi