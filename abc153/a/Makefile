.PHONY: Main
all: Main
	./Main < input1 > output1
	diff expect1 output1 || exit 0
	./Main < input2 > output2
	diff expect2 output2 || exit 0
	./Main < input3 > output3
	diff expect3 output3 || exit 0

Main:
	stack ghc --resolver lts-6.35 -- -O2 Main.hs

.PHONY: clean
clean:
	rm -f Main output* Main.hi Main.o
