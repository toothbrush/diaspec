{-# LANGUAGE RankNTypes #-}

module Diaspec.Backend.GenerateJava where

{-
 - This module is intended to handle generation
 - of the Java classes resulting from a specification.
 -}

-- Copyright 2015 © Paul van der Walt <paul.vanderwalt@inria.fr>

import Diaspec.Backend.AG
import Language.Java.Syntax

-- give this a name for exporting.
genJava :: Specification -> [CompilationUnit] -- CompilationUnit roughly corresponds to a Java file.
genJava s =
  runner_Syn_Specification (wrapAG s)
  : genJ_Syn_Specification   (wrapAG s)

wrapAG s = wrap_Specification
         (sem_Specification s)
         Inh_Specification{}
