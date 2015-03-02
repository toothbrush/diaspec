module Tokens where
import DiaGrammar (Type (..))

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
