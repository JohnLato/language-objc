all:
	chmod 444 input.c
	ghc --make Test.hs
	./Test input.c
clean:
	rm -rf Test.o Test.hi Test
