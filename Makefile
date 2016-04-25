HC=ghc
MAINDIR=/home/chris/School/CS331
SRCDIR=$(MAINDIR)/src
BINDIR=$(MAINDIR)/bin
TESTDIR=$(MAINDIR)/test
HCFLAGS=-O2 -odir $(BINDIR)/objects -hidir $(BINDIR)/interfaces -i$(SRCDIR)

all: bpl

bpl: $(SRCDIR)/bpl.hs $(SRCDIR)/Compiler.hs $(SRCDIR)/Analyzer.hs $(SRCDIR)/Parser.hs $(SRCDIR)/Scanner.hs
	$(HC) $(SRCDIR)/bpl.hs $(HCFLAGS) -o $(MAINDIR)/bpl

Compiler: $(SRCDIR)/Compiler.hs
	$(HC) $(SRCDIR)/Compiler.hs $(HCFLAGS) -o $(MAINDIR)/Compiler

AnalyzerTest: $(SRCDIR)/AnalyzerTest.hs $(SRCDIR)/Analyzer.hs Parser.hs Scanner.hs
	$(HC) $(SRCDIR)/AnalyzerTest.hs $(HCFLAGS) -o $(MAINDIR)/AnalyzerTest

Analyzer: $(SRCDIR)/Analyzer.hs
	$(HC) $(SRCDIR)/Analyzer.hs $(HCFLAGS) -o $(MAINDIR)/Analyzer

Types: $(SRCDIR)/Types.hs
	$(HC) $(SRCDIR)/Types.hs $(HCFLAGS) -o $(MAINDIR)/Types

ParserTest: $(SRCDIR)/ParserTest.hs $(SRCDIR)/Parser.hs $(SRCDIR)/Scanner.hs
	$(HC) $(SRCDIR)/ParserTest.hs $(HCFLAGS) -o $(MAINDIR)/ParserTest

Parser: $(SRCDIR)/Parser.hs
	$(HC) $(SRCDIR)/Parser.hs $(HCFLAGS) -o $(MAINDIR)/Parser

Grammar: $(SRCDIR)/Grammar.hs
	$(HC) $(SRCDIR)/Grammar.hs $(HCFLAGS) -o $(MAINDIR)/Grammar

ScannerTest: $(SRCDIR)/ScannerTest.hs $(SRCDIR)/Scanner.hs $(SRCDIR)/Token.hs
	$(HC) $(SRCDIR)/ScannerTest $(HCFLAGS) -o $(MAINDIR)/ScannerTest

Scanner: $(SRCDIR)/Scanner.hs $(SRCDIR)/Token.hs
	$(HC) $(SRCDIR)/Scanner.hs $(HCFLAGS) -o $(MAINDIR)/Scanner

Token: $(SRCDIR)/Token.hs
	$(HC) $(SRCDIR)/Token.hs $(HCFLAGS) -o $(MAINDIR)/Token

clean:
	rm -f $(MAINDIR)/Token $(MAINDIR)/Scanner $(MAINDIR)/ScannerTest
	rm -f $(MAINDIR)/Grammar $(MAINDIR)/Parser $(MAINDIR)/ParserTest
	rm -f $(MAINDIR)/Types $(MAINDIR)/Analyzer $(MAINDIR)/AnalyzerTest
	rm -f $(MAINDIR)/Compiler $(MAINDIR)/bpl
	rm -f $(MAINDIR)/*.s

rebuild:
	rm -f bin/objects/*
	rm -f bin/interfaces/*

cleartests:
	rm -f $(TESTDIR)/asm/*
	rm -f $(TESTDIR)/bin/*
	rm -f $(TESTDIR)/log/*
	rm -f $(TESTDIR)/out/*
