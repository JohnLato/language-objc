export BINDIR=../../bin
export NO_SEMANTIC_ANALYSIS=1
export CC_FLAGS=-Wall
all: deprecated
deprecated:	
	$(BINDIR)/compile_log.sh deprecated
	$(BINDIR)/compile_log.sh deprecated-2
	$(BINDIR)/compile_log.sh deprecated-3
	$(BINDIR)/compile_log.sh deprecated-bitfield-init
	$(BINDIR)/compile_log.sh fun_decl
clean:
	rm *.log
	