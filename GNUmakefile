AGs = $(shell find src/ -type f -name '*.ag')

all: src/Diaspec/Backend/AG.hs
	cabal build

# This AG file includes all the others.
src/Diaspec/Backend/AG.hs: $(AGs)
	uuagc -Hdpcfws --self src/Diaspec/Backend/AG.ag
