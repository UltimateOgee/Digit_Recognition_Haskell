# Commands:

.PHONY: build init test clean doc deploy stage

build: 
	ghc --make -O -o classifier Main.hs

prof:
	ghc --make -prof -o classifier Main.hs

all: build test

# Cleaning commands:
clean:
	rm -f classifier
	rm -f *.hi
	rm -f *.o

test: build
	./classifier --test

testq: build
	./classifier --test -q

setup:
	cabal update
	cabal install ansi-terminal
