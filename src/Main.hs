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
import System.Environment (getArgs, withArgs)
import System.Exit
import Control.Monad (when)
import Paths_diaspec (version)
import Data.Version (showVersion)

data DSOptions = Pretty { inFile :: FilePath
                        , output :: Maybe FilePath }
               | Racket { inFile :: FilePath
                        , output :: Maybe FilePath }
               | Java   { inFile :: FilePath
                        , outDir :: FilePath }
               deriving (Show, Data, Typeable)

typSpec = typ "SPEC"

pretty = Pretty { inFile = def &= argPos 0 &= typSpec
                , output = Nothing &= outhelp &= typFile
                } &= help "Neaten up a Diaspec file."
java   = Java   { inFile  = def   &= argPos 0 &= typSpec
                , outDir  = "gen" &= typDir &= jdirhelp
                } &= auto
                  &= help "Generate Java framework from spec. (default mode)"
                  &= details  [ "  *Please note:*"
                              , "    Java framework will potentially overwrite"
                              , "    files in the given directory. Default is \"gen/\"."
                              ]
racket = Racket { inFile = def &= argPos 0 &= typSpec
                , output = Nothing &= outhelp &= typFile
                } &= help "Convert Diaspec into Racket spec."

outhelp  = help "Write to FILE."
jdirhelp = help "Place framework in DIR."

mode = cmdArgsMode $
       modes [java, pretty, racket]
       &= program _PROGRAM_NAME
       &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
       &= help _PROGRAM_ABOUT
       &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
       &= helpArg [explicit, name "help", name "h"]

_PROGRAM_NAME = "diaspec"
_PROGRAM_VERSION = showVersion version
_PROGRAM_INFO = _PROGRAM_NAME ++ " v" ++ _PROGRAM_VERSION
_PROGRAM_ABOUT ="Compile Diaspec specifications to various useful formats."
_COPYRIGHT = "© 2015 Paul van der Walt"

getOpts :: IO DSOptions
getOpts = cmdArgsRun mode

main :: IO ()
main = do
  args <- getArgs
  -- If the user did not specify any arguments, pretend "--help" was given
  opts <- (if null args then withArgs ["--help"] else id) getOpts
  optionHandler opts

optionHandler :: DSOptions -> IO ()
optionHandler (Pretty infile out) = do
       putStrLn "[DIASPEC] Pretty-print mode selected."
       handlePretty prettyDia    infile out
optionHandler (Racket infile out) = do
       putStrLn "[RACKET] Pretty-print mode selected."
       handlePretty prettyRacket infile out
optionHandler (Java infile outdir)= do
       putStrLn "[JAVA] Generate framework."
       when (null outdir) $ putStrLn "--outdir blank!" >> exitWith (ExitFailure 1)
    
       handleJava infile outdir

readSomething :: FilePath -> IO (String, String)
readSomething "-" = do putStrLn "Reading from STDIN."
                       spec <- getContents
                       let pn  = "stdin"
                       return (pn, spec)
readSomething i   = do putStrLn$ "Reading from file: " ++ show i
                       putStr "\n\n----\n\n"
                       spec <- readFile i
                       let pn  = filter isAlpha $ takeBaseName i
                       return (pn, spec)

writeStng :: Maybe FilePath -> String -> IO ()
writeStng Nothing _    = return ()
writeStng (Just "-") _ = return () -- we already write to STDOUT
writeStng (Just f)   c = do putStrLn$"----\n\nWriting to file: " ++ show f ++ "."
                            writeFile f c

handleJava :: FilePath -> FilePath -> IO ()
handleJava i   o = do (pn, spec) <- readSomething i
                      let res = ((genJava . S pn) . parseGrammar . alexScanTokens) spec
                          txt = map prettyPrint res
                      --TODO write files into DIR
                      mapM_ putStrLn txt
                      

handlePretty :: (Specification -> PP_Doc) -> FilePath -> Maybe FilePath -> IO ()
handlePretty pf i o = do
  (pn,spec) <- readSomething i
  let res = ((pf . S pn) . parseGrammar . alexScanTokens) spec
  render res 80
  writeStng o (disp res 80 "")
