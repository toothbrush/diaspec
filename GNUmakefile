AGs = $(shell find src/ -type f -name '*.ag')
HSs = $(shell find src/ -type f -name '*.hs')
HandwrittenHS := $(filter-out src/Diaspec/Backend/AG.hs,$(HSs))

all: diaspec
	cabal build

diaspec:
	-ln -sv dist/build/diaspec/diaspec

lint:
	@echo Handwritten .hs = $(HandwrittenHS)
	hlint $(HandwrittenHS)

.PHONY: lint
