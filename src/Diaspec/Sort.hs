-- i know i know
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Diaspec.Sort where

{-
 - Horribly ugly sorting function. What i want is
 - the order sources, actions, contexts, controllers.
 - Same category components should be compared by name.
 -}

-- Copyright © 2015 Paul van der Walt <paul@denknerd.org>

import Diaspec.Backend.AG (Declaration (..))

instance Ord Declaration where
  compare (Source  n1 _   ) (Source n2 _    ) = compare n1 n2
  compare  Source{}          _                = LT
  compare  Action{}          Source{}         = GT
  compare (Action  n1 _   ) (Action n2 _    ) = compare n1 n2
  compare  Action{}          Context{}        = LT
  compare  Action{}          Controller{}     = LT
  compare  Context{}         Source{}         = GT
  compare  Context{}         Action{}         = GT
  compare (Context n1 _ _ ) (Context n2 _ _ ) = compare n1 n2
  compare  Context{}         Controller{}     = LT
  compare (Controller n1 _) (Controller n2 _) = compare n1 n2
  compare  Controller{}      _                = GT
