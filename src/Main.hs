-- Copyright 2015 © Paul van der Walt <paul.vanderwalt@inria.fr>

{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Diaspec.Frontend.Parser (parseGrammar)
import Diaspec.Frontend.Lexer  (alexScanTokens)
import Diaspec.Backend.PrintDiaspec

import UU.PPrint (putDoc)

import System.Console.CmdArgs

data DiaspecCompiler = Pretty { inFile :: FilePath
                              , outFile :: FilePath}
                     | Java   { inFile :: FilePath
                              , outDir :: FilePath}
                     | Racket { inFile :: FilePath
                              , outFile :: FilePath}
                     deriving (Show, Data, Typeable)

-- outFlags x = x &= help "Destination directory" &= typDir

pretty = Pretty { inFile = def &= argPos 0 &= typFile  -- &= help "Specification file to process."
                , outFile = outFileHelp} &= auto
java   = Java   { inFile = def &= argPos 0 &= typFile  -- &= help "Specification file to process."
                , outDir = def &= argPos 1  &= typDir}
racket = Racket { inFile = def &= argPos 0 &= typFile  -- &= help "Specification file to process."
                , outFile = outFileHelp}
  
outFileHelp = "-" &= help "Output file. Default to STDOUT." &= typFile

main :: IO ()
main = do
  opt <- cmdArgs $ modes [pretty, racket, java] &= help "bluh?" &= program "diaspec"
  case opt of
   (Pretty infile out) -> handlePretty infile out
   (_)          -> undefined

handlePretty :: FilePath -> FilePath -> IO ()
handlePretty i o = do
  putStrLn "Pretty-print mode selected."
  spec <- case i of
           "-" -> do putStrLn "Reading from STDIN."
                     getContents
           _   -> do putStrLn ("Dealing with file: " ++ show i)
                     readFile i
  (putDoc . prettyDia . parseGrammar . alexScanTokens) spec
