-- Copyright 2015 © Paul van der Walt <paul.vanderwalt@inria.fr>

{-# LANGUAGE DeriveDataTypeable #-}

-- cmdargs breaks without common subexpression elimination turned off.
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Diaspec.Frontend.Parser (parseGrammar)
import Diaspec.Frontend.Lexer  (alexScanTokens)
import Diaspec.Backend.AG (Specification (..))
import Diaspec.Backend.PrintDiaspec
import Diaspec.Backend.GenerateJava

import Language.Java.Pretty (prettyPrint)
import UU.Pretty (disp, render, PP_Doc (..))

import Data.List.Utils (split)
import Data.Char (isAlpha)
import System.Console.CmdArgs
import System.FilePath

data DiaspecCompiler = Pretty { inFile :: FilePath
                              , outFile :: Maybe FilePath}
                     | Racket { inFile :: FilePath
                              , outFile :: Maybe FilePath}
                     | Java   { inFile :: FilePath }
                             -- , outDir :: FilePath}
                     deriving (Show, Data, Typeable)

pretty = Pretty { inFile = def &= argPos 0 &= typFile
                , outFile = Nothing &= typFile} -- &= explicit &= name "ppspec"
         -- &= auto
java   = Java   { inFile = def &= argPos 0 &= typFile
                           } -- &= explicit &= name "java"
                -- , outDir = def &= argPos 1 &= typDir}
racket = Racket { inFile = def &= argPos 0 &= typFile
                , outFile = Nothing &= typFile} -- &= explicit &= name "racket"

-- outFileHelp = Nothing &= help "Output file. Default to STDOUT." &= typFile

mode = cmdArgsMode $ modes [racket, java, pretty] &= program "diaspec" -- &= help "bluh?"

main :: IO ()
main = do
  opt <- cmdArgsRun mode
  case opt of
   (Pretty infile out) ->
     do
       putStrLn "[DIASPEC] Pretty-print mode selected."
       handlePretty prettyDia    infile out
   (Racket infile out) ->
     do
       putStrLn "[RACKET] Pretty-print mode selected."
       handlePretty prettyRacket infile out
   (Java infile)->
   --(Java infile outdir)->
     do
       putStrLn "[JAVA] Generate framework."
       handleJava infile ""

-- TODO less fugly method of building Specification from [Declaration] :/
handleJava :: FilePath -> FilePath -> IO()
handleJava i o = do
  spec <- case i of
           "-" -> do putStrLn "Reading from STDIN."
                     getContents
           _   -> do putStrLn ("Dealing with file: " ++ show i)
                     putStr "\n\n----\n\n"
                     readFile i
  let pn  = filter isAlpha $ takeBaseName i
  let res = ((genJava . S pn) . parseGrammar . alexScanTokens) spec
  mapM_ (putStrLn . prettyPrint) res

handlePretty :: (Specification -> PP_Doc) -> FilePath -> Maybe FilePath -> IO ()
handlePretty pf i o = do
  spec <- case i of
           "-" -> do putStrLn "Reading from STDIN."
                     getContents
           _   -> do putStrLn ("Dealing with file: " ++ show i)
                     readFile i
  let pn  = filter isAlpha $ takeBaseName i
  let res = ((pf . S pn) . parseGrammar . alexScanTokens) spec
  render res 80
  case o of
    Nothing -> return ()
    Just fname -> do
      writeFile fname (disp res 80 "")
      putStrLn $ "---\n\nWriting to file " ++ show fname ++ "."
