all: src/Diaspec/Backend/AG.hs
	cabal build

src/Diaspec/Backend/AG.hs: src/Diaspec/Backend/AG.ag
	uuagc -Hdpcfws --self src/Diaspec/Backend/AG.ag
