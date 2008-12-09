SCANFILE=../../../examples/ScanFile
CORRECT_CS=good_assign.c good_sinit.c good_sinit2.c good_anonunion.c
ERROR_CS=bad_args.c bad_sinit1.c

export TRACE_EVENTS=1

all: test
test:
	@for f in $(CORRECT_CS); do $(SCANFILE) $$f 2> $$f.log; done
	@for f in $(ERROR_CS); do ( $(SCANFILE) $$f 2> $$f.log; ../expect_error $$? "Expecting error when analysing $$f"); done	
test-gcc:
	@gcc -c $(CORRECT_CS)
	@for f in $(ERROR_CS); do (gcc -c $$f 2>.log ; ../expect_error $$? `cat .log`); done
clean:
	rm -f *.log *.o
