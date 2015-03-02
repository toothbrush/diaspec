module Main where

import Diaspec.Frontend.Parser (parseGrammar)
import Diaspec.Frontend.Lexer  (alexScanTokens)
import Diaspec.Backend.PrintDiaspec

import UU.PPrint (putDoc)

main = getContents >>= putDoc . prettyDia . parseGrammar . alexScanTokens
