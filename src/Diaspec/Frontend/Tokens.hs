-- Copyright 2015 © Paul van der Walt <paul.vanderwalt@inria.fr>
--
module Diaspec.Frontend.Tokens where
import Diaspec.Types (Type (..))

-- The tokens, in a type:
data Token = TokContext
           | TokController
           | TokSource
           | TokAction
           | TokTy Type
           | TokWhenRequired
           | TokWhenProvided
           | TokAlwaysPublish
           | TokMaybePublish
           | TokAs
           | TokGet
           | TokDo
           | TokOpenBr
           | TokCloseBr
           | TokVar String
           deriving (Eq, Show)
