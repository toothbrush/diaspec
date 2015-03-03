{-# OPTIONS_GHC -fno-warn-orphans #-}
module Diaspec.Sort where

import Diaspec.Backend.AG (Declaration (..))

instance Ord Declaration where
  compare (Source n1 _)     (Source n2 _)     = compare n1 n2
  compare (Source _  _)     _                 = LT
  compare (Action _  _)     (Source _  _)     = GT
  compare (Action n1 _)     (Action n2 _)     = compare n1 n2
  compare (Action _ _)      (Context _ _ _)   = LT
  compare (Action _ _)      (Controller _ _)  = LT
  compare (Context _ _ _)   (Source _ _)      = GT
  compare (Context _ _ _)   (Action _ _)      = GT
  compare (Context n1 _ _)  (Context n2 _ _)  = compare n1 n2
  compare (Context _ _ _)   (Controller _ _)  = LT
  compare (Controller n1 _) (Controller n2 _) = compare n1 n2
  compare (Controller _ _)  _                 = GT
