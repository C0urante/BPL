HC=ghc
HCFLAGS=-O2 -odir bin/objects -hidir bin/interfaces

all: bpl

bpl: bpl.hs Compiler.hs Analyzer.hs Parser.hs Scanner.hs
	$(HC) bpl.hs $(HCFLAGS)

Compiler: Compiler.hs
	$(HC) Compiler.hs $(HCFLAGS)

AnalyzerTest: AnalyzerTest.hs Analyzer.hs Parser.hs Scanner.hs
	$(HC) AnalyzerTest.hs $(HCFLAGS)

Analyzer: Analyzer.hs
	$(HC) Analyzer.hs $(HCFLAGS)

Types: Types.hs
	$(HC) Types.hs $(HCFLAGS)

ParserTest: ParserTest.hs Parser.hs Scanner.hs
	$(HC) ParserTest.hs $(HCFLAGS)

Parser: Parser.hs
	$(HC) Parser.hs $(HCFLAGS)

Grammar: Grammar.hs
	$(HC) Grammar.hs $(HCFLAGS)

ScannerTest: ScannerTest.hs Scanner.hs Token.hs
	$(HC) ScannerTest $(HCFLAGS)

Scanner: Scanner.hs Token.hs
	$(HC) Scanner.hs $(HCFLAGS)

Token: Token.hs
	$(HC) Token.hs $(HCFLAGS)

clean:
	rm -f Token Scanner ScannerTest
	rm -f Grammar Parser ParserTest
	rm -f Types Analyzer AnalyzerTest
	rm -f Compiler bpl
	rm -f *.s
	rm -f *.out

rebuild:
	rm -f bin/objects/*
	rm -f bin/interfaces/*
