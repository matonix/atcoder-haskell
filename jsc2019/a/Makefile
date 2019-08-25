.PHONY: Main
all: Main
	./Main < input1 > output1
	diff expect1 output1
	./Main < input2 > output2
	diff expect2 output2

Main:
	stack ghc --resolver ghc-7.10.3 -- -O2 Main.hs

.PHONY: clean
clean:
	rm -f Main output* Main.hi Main.o
