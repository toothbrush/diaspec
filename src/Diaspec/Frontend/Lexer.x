{
-- Copyright 2015 © Paul van der Walt <paul.vanderwalt@inria.fr>

module Diaspec.Frontend.Lexer (alexScanTokens, Token (..), AlexPosn (..), token_posn) where
import Diaspec.Types (Type (..))
}

%wrapper "posn"

$digit 	 = [0-9]		-- digits
$alpha 	 = [a-zA-Z]		-- alphabetic characters
$alphaUp = [A-Z]		-- alphabetic characters, uppercase

tokens :-

  $white+				;
  "--".*				;
  context				{ \p s -> TokContext       p         }
  controller                            { \p s -> TokController    p         }
  source                                { \p s -> TokSource        p         }
  action                                { \p s -> TokAction        p         }
  when\_required                        { \p s -> TokWhenRequired  p         }
  when\_provided                        { \p s -> TokWhenProvided  p         }
  always\_publish                       { \p s -> TokAlwaysPublish p         }
  maybe\_publish                        { \p s -> TokMaybePublish  p         }
  Bool                                  { \p s -> TokTy            p Bool    }
  Int                                   { \p s -> TokTy            p Int     }
  String                                { \p s -> TokTy            p String  }
  Picture                               { \p s -> TokTy            p Picture }
  as                                    { \p s -> TokAs            p         }
  get                                   { \p s -> TokGet           p         }
  do                                    { \p s -> TokDo            p         }
  "{"                                   { \p s -> TokOpenBr        p         }
  "}"                                   { \p s -> TokCloseBr       p         }
  $alphaUp [$alpha $digit]*             { \p s -> TokVar           p s       }

{
-- Each action has type :: AlexPosn -> String -> Token

-- The tokens, in a type:
data Token = TokContext       AlexPosn
           | TokController    AlexPosn
           | TokSource        AlexPosn
           | TokAction        AlexPosn
           | TokTy            AlexPosn Type
           | TokWhenRequired  AlexPosn
           | TokWhenProvided  AlexPosn
           | TokAlwaysPublish AlexPosn
           | TokMaybePublish  AlexPosn
           | TokAs            AlexPosn
           | TokGet           AlexPosn
           | TokDo            AlexPosn
           | TokOpenBr        AlexPosn
           | TokCloseBr       AlexPosn
           | TokVar           AlexPosn String
           deriving (Eq, Show)

token_posn :: Token -> AlexPosn
token_posn t = undefined

}
