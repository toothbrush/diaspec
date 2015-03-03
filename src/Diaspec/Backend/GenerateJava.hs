{-# LANGUAGE RankNTypes #-}

module Diaspec.Backend.GenerateJava where

{-
 - This module is intended to handle generation
 - of the Java classes resulting from a specification.
 -}

-- Copyright 2015 © Paul van der Walt <paul.vanderwalt@inria.fr>

import Diaspec.Backend.AG
import Diaspec.Sort ()
import Data.List (sort)
import Language.Java.Syntax

-- give this a name for exporting.
genJava :: Specification -> [CompilationUnit] -- CompilationUnit roughly corresponds to a Java file.
genJava s = map (\d -> genJ_Syn_Declaration (wrap_Declaration (sem_Declaration d) inh_Declaration)) (sort s)

inh_Declaration :: Inh_Declaration
inh_Declaration = Inh_Declaration {}

