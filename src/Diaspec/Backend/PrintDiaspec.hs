{-# LANGUAGE RankNTypes #-}

module Diaspec.Backend.PrintDiaspec where

{-
 - This module is intended to handle pretty printing of
 - Diaspec code. Using the compiler using this backend
 - should be like a linter.
 -}

-- Copyright 2015 © Paul van der Walt <paul.vanderwalt@inria.fr>

import UU.Pretty hiding (pp)
import Diaspec.Backend.AG
import Diaspec.Sort ()
import Data.List (sort)

-- give this a name for exporting.
prettyDia :: Specification -> PP_Doc
prettyDia s = text "" >-< text "-- Auto generated specification" >-<
              text "" >-< format (sort s)
  where format = formatWith (\d -> 
                              ppDia_Syn_Declaration (wrap_Declaration (sem_Declaration d) inhDeclaration))

inhDeclaration :: Inh_Declaration
inhDeclaration = Inh_Declaration {}

formatWith :: forall a b. (PP b) => (a->b) -> [a] -> PP_Doc
formatWith f = foldr (\d doc -> f d >-< text "" >-< doc) empty

prettyRacket :: Specification -> PP_Doc
prettyRacket s = text "" >-< text ";; Auto generated specification" >-<
                 text "#lang s-exp \"diaspec.rkt\"" >-<
                 text "" >-< format (sort s)
        where format = formatWith (\d ->
                                    ppRacket_Syn_Declaration (wrap_Declaration (sem_Declaration d) inhDeclaration))
