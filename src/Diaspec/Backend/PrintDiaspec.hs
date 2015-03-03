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

--import UU.PPrint
import UU.Pretty
import Diaspec.Backend.AG

instance PP Declaration where
  pp d = ppDia_Syn_Declaration (wrap_Declaration (sem_Declaration d) inh_Declaration)

-- give this a name for exporting.
prettyDia :: Specification -> PP_Doc
prettyDia []      = text "[]"
prettyDia (d: ds) = pp d >-< pp ds

inh_Declaration :: Inh_Declaration
inh_Declaration = Inh_Declaration {}
