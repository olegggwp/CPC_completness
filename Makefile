# HC=/opt/ghc/7.10.3/bin/ghc
HC=ghc
SOURCES=src/Main.hs
PACKAGE=hw0.zip
OUT=parser

.PHONY: all run clean pack

all: $(OUT)

run: $(OUT)
	./$(OUT)

clean:
	rm -rf src/*.o src/*.hi $(OUT)

$(OUT): $(SOURCES)
	$(HC) -i./src -tmpdir . $(SOURCES) -o $(OUT)

pack:
	zip $(PACKAGE) -r Makefile src

test: $(OUT)
	./$(OUT) < testB1
	./$(OUT) < testB2
	./$(OUT) < testB3
	# ./$(OUT) < test1
	# ./$(OUT) < test2
	# ./$(OUT) < parsetest