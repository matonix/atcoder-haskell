.PHONY: Main
all: Main
	./Main < 1.input > 1.output
	diff 1.expect 1.output
	./Main < 2.input > 2.output
	diff 2.expect 2.output
	./Main < 3.input > 3.output
	diff 3.expect 3.output

Main:
	stack ghc --resolver ghc-7.10.3 -- -O2 Main.hs
	rm Main.hi Main.o

.PHONY: clean
clean:
	rm -f Main *.output
