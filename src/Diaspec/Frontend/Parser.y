{
-- Copyright 2015 © Paul van der Walt <paul.vanderwalt@inria.fr>

module Diaspec.Frontend.Parser where
import Diaspec.Frontend.Lexer
import Diaspec.Backend.AG

}

%name parseGrammar
%tokentype { Token }
%error { happyError }

-- define all possible tokens, aliases
%token 
      context         { TokContext       _    }
      controller      { TokController    _    }
      source          { TokSource        _    }
      action          { TokAction        _    }
      type            { TokTy            _ $$ }
      whenrequired    { TokWhenRequired  _    }
      whenprovided    { TokWhenProvided  _    }
      alwayspublish   { TokAlwaysPublish _    }
      maybepublish    { TokMaybePublish  _    }
      as              { TokAs            _    }
      get             { TokGet           _    }
      do              { TokDo            _    }
      '{'             { TokOpenBr        _    }
      '}'             { TokCloseBr       _    }
      var             { TokVar           _ $$ }

-- a %% for no real reason

%%

-- production rules for the grammar

prods :: { [Declaration] }
prods : Exp                    { [$1]    }
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
happyError :: [Token] -> a
happyError tks = error ("Parse error at " ++ lcn ++ "\n")
      where
      lcn = case tks of
              [] -> "end of file"
              tk:_ -> "line " ++ show l ++ ", column " ++ show c
                   where
                      AlexPn _ l c = token_posn tk

}
