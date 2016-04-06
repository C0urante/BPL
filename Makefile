HC=ghc
HCFLAGS=-O2

all: ScannerTest ParserTest AnalyzerTest

AnalyzerTest: AnalyzerTest.hs
	$(HC) AnalyzerTest.hs $(HCFLAGS)

Analyzer: Analyzer.hs
	$(HC) Analyzer.hs $(HCFLAGS)

Environment: Environment.hs
	$(HC) Environmeht.hs $(HCFLAGS)

ParserTest: ParserTest.hs
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

simple:
	rm -f *.o *.hi

clean: simple
	rm -f Token Scanner ScannerTest Grammar Parser ParserTest Environment Analyzer AnalyzerTest
