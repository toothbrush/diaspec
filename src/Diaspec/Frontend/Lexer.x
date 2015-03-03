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
  context				{ tok (\p s -> TokContext 	p         ) }
  controller                            { tok (\p s -> TokController    p         ) }
  source                                { tok (\p s -> TokSource        p         ) }
  action                                { tok (\p s -> TokAction        p         ) }
  when\_required                        { tok (\p s -> TokWhenRequired  p         ) }
  when\_provided                        { tok (\p s -> TokWhenProvided  p         ) }
  always\_publish                       { tok (\p s -> TokAlwaysPublish p         ) }
  maybe\_publish                        { tok (\p s -> TokMaybePublish  p         ) }
  Bool                                  { tok (\p s -> TokTy            p Bool    ) }
  Int                                   { tok (\p s -> TokTy            p Int     ) }
  String                                { tok (\p s -> TokTy            p String  ) }
  Picture                               { tok (\p s -> TokTy            p Picture ) }
  as                                    { tok (\p s -> TokAs            p         ) }
  get                                   { tok (\p s -> TokGet           p         ) }
  do                                    { tok (\p s -> TokDo            p         ) }
  "{"                                   { tok (\p s -> TokOpenBr        p         ) }
  "}"                                   { tok (\p s -> TokCloseBr       p         ) }
  $alphaUp [$alpha $digit]*             { tok (\p s -> TokVar           p s       ) }

{
-- Each action has type :: AlexPosn -> String -> Token

-- Some action helpers:
tok f p s = f p s

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
