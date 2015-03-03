module Main where

import Language.Java.Parser
import Language.Java.Syntax
import Language.Java.Pretty


main = do ast <- parser compilationUnit `fmap` readFile "AbstractCamera.java"
          case ast of
             Left a -> error "die"
             Right pr ->
               do  putStrLn $ prettyPrint pr
                   putStrLn "\n\nBecomes:\n\n"
                   putStrLn $ show pr
