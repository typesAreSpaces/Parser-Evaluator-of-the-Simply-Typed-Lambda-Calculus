all: build clean

build:
	ghc -o runTLBN runTLBN.hs
clean:
	rm *.hi *.o
