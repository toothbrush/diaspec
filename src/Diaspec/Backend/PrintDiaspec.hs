-- i know i know
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-  # LANGUAGE TypeSynonymInstances,
             FlexibleInstances #   -}
module Diaspec.Backend.PrintDiaspec where

{-
 - This module is intended to handle pretty printing of
 - Diaspec code. Using the compiler using this backend
 - should be like a linter.
 -}

-- Copyright 2015 © Paul van der Walt <paul.vanderwalt@inria.fr>

import UU.PPrint
import Diaspec.Backend.AG

instance Pretty Declaration where
  pretty d = ppDia_Syn_Declaration (wrap_Declaration (sem_Declaration d) inh_Declaration)

-- give this a name for exporting.
prettyDia :: Specification -> Doc
prettyDia [] = string "[]"
prettyDia (d: ds) = pretty (d:ds)

inh_Declaration :: Inh_Declaration
inh_Declaration = Inh_Declaration {}
