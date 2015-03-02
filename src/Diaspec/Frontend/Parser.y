{
module Diaspec.Frontend.Parser where
import Diaspec.Frontend.Tokens
import Diaspec.Frontend.Lexer
import Diaspec.Types
  
}

%name parseGrammar
%tokentype { Token }
%error { parseError }

-- define all possible tokens, aliases
%token 
      context         { TokContext       }
      controller      { TokController    }
      source          { TokSource        }
      action          { TokAction        }
      type            { TokTy $$         }
      whenrequired    { TokWhenRequired  }
      whenprovided    { TokWhenProvided  }
      alwayspublish   { TokAlwaysPublish }
      maybepublish    { TokMaybePublish  }
      as              { TokAs            }
      get             { TokGet           }
      do              { TokDo            }
      '{'             { TokOpenBr        }
      '}'             { TokCloseBr       }
      var             { TokVar $$        }

-- a %% for no real reason

%%

-- production rules for the grammar

prods :: { Specification }
prods : Exp                    { [$1] }
      | prods Exp              { $2 : $1 }

Exp :: { Declaration }
Exp    : source     var as type                  { Source $2 $4 }
       | action     var as type                  { Action $2 $4 }
       | context    var as type '{' CtxIntr  '}' { Context $2 $4 $6 }
       | controller var         '{' CtrlIntr '}' { Controller $2 $4 }

CtrlIntr :: { ControllerInteraction }
CtrlIntr : whenprovided var do var          { WhenContext $2 $4 }

CtxIntr :: { ContextInteraction }
CtxIntr : whenprovided var Get Publish      { WhenProvided $2 $3 $4 }
        | whenrequired     Get              { WhenRequired $2 }

Get :: { Get }
Get :  {- empty -}   { Nothing }
    |  get var       { Just $2 }

Publish :: { Publish }
Publish : alwayspublish { AlwaysPublish }
        | maybepublish  { MaybePublish  }

{

-- TODO nicer error.
parseError :: [Token] -> a
parseError _ = error "Parse error"

}
