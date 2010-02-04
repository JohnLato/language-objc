all:
	ghc --make -O2  -prof Test.hs
	./Test > test.c
	gcc -o test_out test.c
	./test_out > test.out
	diff test.expect test.out
clean:
	rm -rf Test.o Test.hi Test test_out
