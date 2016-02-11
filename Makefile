all: ScannerTest

ScannerTest: ScannerTest.hs Scanner.hs Token.hs
	ghc ScannerTest -O2

Scanner: Scanner.hs Token.hs
	ghc Scanner.hs -O2

Token: Token.hs
	ghc Token.hs -O2

cleanup:
	rm -f *.o *.hi

clean: cleanup
	rm -f Token Scanner ScannerTest
