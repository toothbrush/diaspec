AGs = $(shell find src/ -type f -name '*.ag')

all: src/Diaspec/Backend/AG.hs
	@echo All AGs found= $(AGs)
	cabal build

src/Diaspec/Backend/AG.hs: $(AGs)
	uuagc -Hdpcfws --self src/Diaspec/Backend/AG.ag
