all: dist/setup-config
	cabal build

# Cabal actually tries to detect changes.
dist/setup-config: sudoku.cabal
	cabal configure

clean:
	cabal clean

prof:
	./archive +RTS -hy -s
	hp2ps -c archive.hp
	ps2pdf archive.ps
	rm archive.hp
	rm archive.ps
	mv archive.pdf /tmp

.PHONY: all clean
