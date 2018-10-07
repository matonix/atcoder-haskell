.PHONY: Main.out output
all: Main.out output
	diff expect output

Main.out:
	stack ghc --resolver ghc-7.10.3 -- -o Main.out -O2 Main.hs

output:
	./Main.out < input > output

.PHONY: clean
clean:
	rm -f Main.out output Main.hi Main.o