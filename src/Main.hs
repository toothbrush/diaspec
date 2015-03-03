-- Copyright 2015 © Paul van der Walt <paul.vanderwalt@inria.fr>

{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Diaspec.Frontend.Parser (parseGrammar)
import Diaspec.Frontend.Lexer  (alexScanTokens)
import Diaspec.Backend.PrintDiaspec

import UU.PPrint (putDoc)

import System.Console.CmdArgs

data Backends = PPDiaspec
              | RacketSpec
              | JavaFW
                deriving (Show, Eq, Data, Typeable)

data CmdOptions = CmdOptions { inputFile :: String
                             , mode      :: Backends}
                  deriving (Show, Data, Typeable)

defaultOpts = CmdOptions { inputFile = def
                         , mode = PPDiaspec}

main :: IO ()
main = do
  cmdArgs defaultOpts
  txt <- getContents
  (putDoc . prettyDia . parseGrammar . alexScanTokens) txt
