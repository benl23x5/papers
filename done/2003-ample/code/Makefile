

all	: bin/ample.bin

src/Parser.hs	: src/Parser.y
	happy -a -g -c src/Parser.y -isrc/Parser.info -osrc/Parser.hs

sources		= $(wildcard src/*.hs)
ghcFlags	= -fglasgow-exts

bin/ample.bin	: src/Parser.hs $(sources)
	ghc $(ghcFlags) -isrc --make src/Main.hs -o bin/ample.bin

clean	: 
	rm -f `find . 	-name "*~" \
		-or 	-name "*.hi" \
		-or	-name "*.o" \
		-or	-name "DEADJOE"`

	rm -f src/Parser.hs
	rm -f src/Parser.info
	rm -f prof/*
	rm -f bin/ample.bin
			
