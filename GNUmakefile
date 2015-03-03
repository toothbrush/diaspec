AGs = $(shell find src/ -type f -name '*.ag')
HSs = $(shell find src/ -type f -name '*.hs')
HandwrittenHS := $(filter-out src/Diaspec/Backend/AG.hs,$(HSs))


all: src/Diaspec/Backend/AG.hs
	cabal build

# This AG file includes all the others.
src/Diaspec/Backend/AG.hs: $(AGs)
	uuagc -Hdpcfws --self src/Diaspec/Backend/AG.ag

lint:
	@echo All .hs files   = $(HSs)
	@echo Handwritten .hs = $(HandwrittenHS)
	hlint $(HandwrittenHS)


.PHONY: lint
