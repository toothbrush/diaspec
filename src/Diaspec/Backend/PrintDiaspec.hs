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

import Diaspec.Types

  --TODO
instance Pretty Declaration where
  pretty (Source n t) = string "source"
  pretty (Action n t) = string "action"
  pretty (Context n t i) = string "context"
  pretty (Controller n i) = string "controller"

-- give this a name for exporting.
prettyDia :: Specification -> Doc
prettyDia = pretty
