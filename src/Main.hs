-- Copyright 2015 © Paul van der Walt <paul.vanderwalt@inria.fr>

{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Diaspec.Frontend.Parser (parseGrammar)
import Diaspec.Frontend.Lexer  (alexScanTokens)
import Diaspec.Backend.AG (Specification (..))
import Diaspec.Backend.PrintDiaspec
import Diaspec.Backend.GenerateJava

import Language.Java.Pretty (prettyPrint)
import UU.Pretty (disp, render, PP_Doc (..))

import System.Console.CmdArgs

data DiaspecCompiler = Pretty { inFile :: FilePath
                              , outFile :: Maybe FilePath}
                     | Java   { inFile :: FilePath }
                             -- , outDir :: FilePath}
                     | Racket { inFile :: FilePath
                              , outFile :: Maybe FilePath}
                     deriving (Show, Data, Typeable)

pretty = Pretty { inFile = "-" &= argPos 0 &= typFile  -- &= help "Specification file to process."
                , outFile = outFileHelp} -- &= auto
java   = Java   { inFile = "-" &= argPos 0 &= typFile  -- &= help "Specification file to process."
                           }
                -- , outDir = def &= argPos 1 &= typDir}
racket = Racket { inFile = "-" &= argPos 0 &= typFile  -- &= help "Specification file to process."
                , outFile = outFileHelp}

outFileHelp = Nothing &= help "Output file. Default to STDOUT." &= typFile

main :: IO ()
main = do
  opt <- cmdArgs $ modes [pretty, racket, java] &= help "bluh?" &= program "diaspec"
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

handleJava :: FilePath -> FilePath -> IO()
handleJava i o = do
  spec <- case i of
           "-" -> do putStrLn "Reading from STDIN."
                     getContents
           _   -> do putStrLn ("Dealing with file: " ++ show i)
                     putStr "\n\n----\n\n"
                     readFile i
  let res = (genJava . parseGrammar . alexScanTokens) spec
  mapM_ (putStrLn . prettyPrint) res

handlePretty :: (Specification -> PP_Doc) -> FilePath -> Maybe FilePath -> IO ()
handlePretty pf i o = do
  spec <- case i of
           "-" -> do putStrLn "Reading from STDIN."
                     getContents
           _   -> do putStrLn ("Dealing with file: " ++ show i)
                     readFile i
  let res = (pf . parseGrammar . alexScanTokens) spec
  render res 80
  case o of
    Nothing -> return ()
    Just fname -> do
      writeFile fname (disp res 80 "")
      putStrLn $ "---\n\nWriting to file " ++ show fname ++ "."
