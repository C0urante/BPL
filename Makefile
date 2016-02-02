all: Scanner

Scanner: Scanner.hs Token.hs
	ghc Scanner.hs

cleanup:
	rm -f *.o *.hi
