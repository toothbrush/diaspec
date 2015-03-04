{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Language.Java.Parser
import Language.Java.Syntax
import Language.Java.Pretty

import System.Console.CmdArgs

  
data Test = Test { inFile :: FilePath }
                     deriving (Show, Data, Typeable)

test = Test { inFile = def &= args &= typFile }



main = do
  opt <- cmdArgs test
  case opt of
   Test i -> do
      ast <- parser compilationUnit `fmap` readFile i
      case ast of
         Left a -> error "die"
         Right pr ->
           do  putStrLn $ prettyPrint pr
               putStrLn "\n\nBecomes:\n\n"
               putStrLn $ show pr
