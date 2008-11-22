SCANFILE=../../../examples/ScanFile
all:
	$(SCANFILE) ident_kinds.c > ident_kinds.out
	diff -u ident_kinds.expect ident_kinds.out
	$(SCANFILE) tentative.c > tentative.out
	diff -u tentative.expect tentative.out
clean:
	rm -rf *.out