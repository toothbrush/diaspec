{
-- Copyright 2015 © Paul van der Walt <paul.vanderwalt@inria.fr>

module Diaspec.Frontend.Lexer (alexScanTokens) where
import Diaspec.Frontend.Tokens
import Diaspec.Types (Type (..))
}

%wrapper "basic"

$digit 	 = [0-9]		-- digits
$alpha 	 = [a-zA-Z]		-- alphabetic characters
$alphaUp = [A-Z]		-- alphabetic characters, uppercase

tokens :-

  $white+				;
  "--".*				;
  context				{ \s -> TokContext 	}
  controller				{ \s -> TokController 	}
  source				{ \s -> TokSource 	}
  action				{ \s -> TokAction 	}
  when\_required			{ \s -> TokWhenRequired }
  when\_provided			{ \s -> TokWhenProvided }
  always\_publish			{ \s -> TokAlwaysPublish}
  maybe\_publish			{ \s -> TokMaybePublish	}
  Bool  				{ \s -> TokTy Bool 	}
  Int					{ \s -> TokTy Int 	}
  String				{ \s -> TokTy String 	}
  Picture				{ \s -> TokTy Picture 	}
  as					{ \s -> TokAs 		}
  get					{ \s -> TokGet 		}
  do					{ \s -> TokDo 		}
  "{"					{ \s -> TokOpenBr 	}
  "}"					{ \s -> TokCloseBr 	}
  $alphaUp [$alpha $digit]*		{ \s -> TokVar s 	}

{
-- Each action has type :: String -> Token


}
