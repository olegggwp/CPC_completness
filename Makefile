
# HC=/opt/ghc/7.10.3/bin/ghc
HC=ghc
# SOURCES=src/DMain.hs src/Term.hs src/PrintUtils.hs src/Parser.hs src/RTL.hs src/TrueChecker.hs
SOURCES=src/Combined.hs
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
	# ./$(OUT) < to1
	./$(OUT) < Dtest1
	# ./$(OUT) < Dtest2
	# ./$(OUT) < Dtest4
	# ./$(OUT) < Dtest5
	# ./$(OUT) < mergeTest
	# ./$(OUT) < Dtest3
	# ./$(OUT) < testB1
	# ./$(OUT) < testB2
	# ./$(OUT) < testB3
	# ./$(OUT) < test4
	# ./$(OUT) < test1
	# ./$(OUT) < test5
	# ./$(OUT) < test6
	# ./$(OUT) < test2
	# ./$(OUT) < parsetest
# HC=ghc
# SOURCES=src/DMain.hs
# GEN_SOURCES=src/Lexer.x src/Parser.y
# GENERATED=src/Lexer.hs src/Parser.hs
# PACKAGE=hw0.zip

# .PHONY: pack all run clean

# all: parser

# run: parser
# 	./parser

# clean:
# 	rm -rf src/*.o src/*.hi
# 	rm -f parser

# parser:  $(SOURCES)
# 	$(HC) -i./src -tmpdir . ./src/DMain.hs -o parser


# pack:
# 	zip $(PACKAGE) -r Makefile src



# # HC=/opt/ghc/8.0.2/bin/ghc
# # HC=ghc
# # SOURCES=src/Main.hs
# # SOURCES=src/DMain.hs
# # PACKAGE=hw0.zip
# # OUT=parser

# # .PHONY: all run clean pack

# # all: $(OUT)

# # run: $(OUT)
# # 	./$(OUT)

# # clean:
# # 	rm -rf src/*.o src/*.hi $(OUT)

# # $(OUT): $(SOURCES)
# # 	$(HC) -i./src -tmpdir . $(SOURCES) -o $(OUT)

# # pack:
# # 	zip $(PACKAGE) -r Makefile src

# # test: $(OUT)
# # 	./$(OUT) < testB1
# # 	./$(OUT) < testB2
# # 	./$(OUT) < testB3
# # ./$(OUT) < test4
# # ./$(OUT) < test1
# # ./$(OUT) < test5
# # ./$(OUT) < test6
# # ./$(OUT) < test2
# # ./$(OUT) < parsetest

# # HC=/opt/ghc/7.10.3/bin/ghc
# # HC=ghc
# # SOURCES=src/DMain.hs src/Term.hs src/PrintUtils.hs src/Parser.hs src/RTL.hs src/TrueChecker.hs
# # PACKAGE=hw0.zip
# # OUT=parser

# # .PHONY: all run clean pack

# # all: $(OUT)

# # run: $(OUT)
# # 	./$(OUT)

# # clean:
# # 	rm -rf src/*.o src/*.hi $(OUT)

# # $(OUT): $(SOURCES)
# # 	$(HC) -i./src -tmpdir . $(SOURCES) -o $(OUT)

# # pack:
# # 	zip $(PACKAGE) -r Makefile src

# # test: $(OUT)
# # 	# ./$(OUT) < to1
# # 	# ./$(OUT) < Dtest1
# # 	./$(OUT) < Dtest2
# # 	# ./$(OUT) < Dtest4
# # 	# ./$(OUT) < mergeTest
# # 	# ./$(OUT) < Dtest3
# # 	# ./$(OUT) < testB1
# # 	# ./$(OUT) < testB2
# # 	# ./$(OUT) < testB3
# # 	# ./$(OUT) < test4
# # 	# ./$(OUT) < test1
# # 	# ./$(OUT) < test5
# # 	# ./$(OUT) < test6
# # 	# ./$(OUT) < test2
# # 	# ./$(OUT) < parsetest