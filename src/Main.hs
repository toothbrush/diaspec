module Main where

import Parser (parseGrammar)
import Lexer  (alexScanTokens)

main = getContents >>= print . parseGrammar . alexScanTokens
