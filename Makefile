## we've cabalised so this is unused.
EXEC = Main

all: Lexer.hs Parser.hs $(EXEC)
	ghc --make $<

# warning fugly TODO XXX
$(EXEC): *.hs
	ghc --make $(EXEC).hs -o $(EXEC)

Lexer.hs: Lexer.x
	alex $^

Parser.hs: Parser.y
	happy $^
