export BINDIR=../../bin
export NO_SEMANTIC_ANALYSIS=1
export CC_FLAGS=-Wall
all: deprecated
deprecated:	
	sh $(BINDIR)/compile_log.sh deprecated
	sh $(BINDIR)/compile_log.sh deprecated-2
	sh $(BINDIR)/compile_log.sh deprecated-3
	sh $(BINDIR)/compile_log.sh deprecated-bitfield-init
#	$(BINDIR)/compile_log.sh fun_decl # fails
clean:
	rm *.log
	