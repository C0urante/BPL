HC = ghc
MAINDIR = $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
SRCDIR = $(MAINDIR)/src
BINDIR = $(MAINDIR)/bin
TESTDIR = $(MAINDIR)/test
HCFLAGS = -O2 -odir $(BINDIR)/objects -hidir $(BINDIR)/interfaces -i$(SRCDIR)
OUTFLAG = -o $(MAINDIR)/$@

all: bpl

bpl: $(SRCDIR)/bpl.hs $(SRCDIR)/Optimizer.hs $(SRCDIR)/Compiler.hs $(SRCDIR)/Analyzer.hs $(SRCDIR)/Parser.hs $(SRCDIR)/Scanner.hs
	$(HC) $(SRCDIR)/bpl.hs $(HCFLAGS) $(OUTFLAG)

Compiler: $(SRCDIR)/Compiler.hs
	$(HC) $(SRCDIR)/Compiler.hs $(HCFLAGS) $(OUTFLAG)

OptimizerTest: $(SRCDIR)/OptimizerTest.hs $(SRCDIR)/Optimizer.hs $(SRCDIR)/Analyzer.hs $(SRCDIR)/Parser.hs $(SRCDIR)/Scanner.hs
	$(HC) $(SRCDIR)/OptimizerTest.hs $(HCFLAGS) $(OUTFLAG)

Optimizer: $(SRCDIR)/Optimizer.hs
	$(HC) $(SRCDIR)/Optimizer.hs $(HCFLAGS) $(OUTFLAG)

AnalyzerTest: $(SRCDIR)/AnalyzerTest.hs $(SRCDIR)/Analyzer.hs $(SRCDIR)/Parser.hs $(SRCDIR)/Scanner.hs
	$(HC) $(SRCDIR)/AnalyzerTest.hs $(HCFLAGS) $(OUTFLAG)

Analyzer: $(SRCDIR)/Analyzer.hs
	$(HC) $(SRCDIR)/Analyzer.hs $(HCFLAGS)

Types: $(SRCDIR)/Types.hs
	$(HC) $(SRCDIR)/Types.hs $(HCFLAGS)

ParserTest: $(SRCDIR)/ParserTest.hs $(SRCDIR)/Parser.hs $(SRCDIR)/Scanner.hs
	$(HC) $(SRCDIR)/ParserTest.hs $(HCFLAGS) $(OUTFLAG)

Parser: $(SRCDIR)/Parser.hs
	$(HC) $(SRCDIR)/Parser.hs $(HCFLAGS)

Grammar: $(SRCDIR)/Grammar.hs
	$(HC) $(SRCDIR)/Grammar.hs $(HCFLAGS)

ScannerTest: $(SRCDIR)/ScannerTest.hs $(SRCDIR)/Scanner.hs $(SRCDIR)/Token.hs
	$(HC) $(SRCDIR)/ScannerTest $(HCFLAGS) $(OUTFLAG)

Scanner: $(SRCDIR)/Scanner.hs $(SRCDIR)/Token.hs
	$(HC) $(SRCDIR)/Scanner.hs $(HCFLAGS)

Token: $(SRCDIR)/Token.hs
	$(HC) $(SRCDIR)/Token.hs $(HCFLAGS)

clean:
	rm -f $(MAINDIR)/Token $(MAINDIR)/Scanner $(MAINDIR)/ScannerTest
	rm -f $(MAINDIR)/Grammar $(MAINDIR)/Parser $(MAINDIR)/ParserTest
	rm -f $(MAINDIR)/Types $(MAINDIR)/Analyzer $(MAINDIR)/AnalyzerTest
	rm -f $(MAINDIR)/Optimizer $(MAINDIR)/OptimizerTest
	rm -f $(MAINDIR)/Compiler $(MAINDIR)/bpl
	rm -f $(MAINDIR)/*.s

rebuild:
	rm -f $(MAINDIR)/bin/objects/*
	rm -f $(MAINDIR)/bin/interfaces/*

cleartests:
	rm -f $(TESTDIR)/asm/*
	rm -f $(TESTDIR)/bin/*
	rm -f $(TESTDIR)/log/*
	rm -f $(TESTDIR)/out/*
