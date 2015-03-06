module Diaspec.JTypeUtils where

import Language.Java.Syntax
import Data.String.Utils (split)

funcparams :: [(RefType, String)] -> [FormalParam]
funcparams = map (\(t,n) -> FormalParam [] (RefType t) False (VarId (Ident n)))


absRunCls :: RefType
absRunCls = (ClassRefType (ClassType [(Ident "AbstractRunner",[])]))


runnerInit :: Bool -- ^ should it be Final?
           -> [Decl]
runnerInit b    = [ MemberDecl (FieldDecl (runnerVarModifs b) (RefType absRunCls)
                                [VarDecl (VarId (Ident "runner")) Nothing])
                  , methodDecl (runnerMethModifs b) Nothing
                    "init"
                    (funcparams$[(absRunCls, "runner")])
                    (Just (Block [BlockStmt
                                  (ExpStmt (Assign (FieldLhs (PrimaryFieldAccess This (Ident "runner")))
                                            EqualA (ExpName (Name [Ident "runner"]))))]))
                  ]
  where runnerMethModifs True  = [Final, Protected]
        runnerMethModifs False = [Protected]
        runnerVarModifs  True  = [Private]
        runnerVarModifs  False = [Protected]


pack :: [Char] -> Maybe PackageDecl
pack n = packagify $ concat ["fr.diaspec.", n, ".generated"]
  where packagify s = Just (PackageDecl (Name (map Ident (split "." s))))

stmts :: [BlockStmt] -> Maybe Block
stmts = Just . Block

lVar :: Type -> String -> Maybe VarInit -> BlockStmt
lVar ty nm value = LocalVars [] ty [VarDecl (VarId (Ident nm)) value]

proxyClGet :: [Char] -> String -> Maybe Type -> Decl
proxyClGet srcName proxyName srcTy =
  proxyCl proxyName srcTy ("query" ++ srcName ++ "Value")
  []
  [BlockStmt (IfThenElse
              (ExpName (Name [Ident "isAccessible"]))
              (StmtBlock (Block
                          [BlockStmt
                           (Return (Just
                                    (MethodInv
                                     (PrimaryMethodCall
                                      (MethodInv (MethodCall
                                                  (Name [Ident "runner",Ident$"get"++srcName]) [])) []
                                      (Ident "requireValue") []))))]))
              (StmtBlock (Block
                          [BlockStmt
                           (Throw (InstanceCreation []
                                   (ClassType [(Ident "RuntimeException",[])])
                                   [Lit (String$"Access forbidden for "++srcName++" source")] Nothing))])))]

methodDecl :: [Modifier] -- ^ scope modifiers, abstract, final
           -> Maybe Type -- ^ expected return type.
           -> String       -- ^ desired method name
           -> [FormalParam] -- ^ method arguments
           -> Maybe Block
           -> Decl -- ^ a method declaration.
methodDecl scope ret nm args bod =
  MemberDecl$ MethodDecl
  scope [] ret (Ident nm) args
  [] -- no exceptions
  (MethodBody bod)

proxyClDo :: String -> String -> [FormalParam] -> Decl
proxyClDo actName proxyName methArgs =
  proxyCl proxyName Nothing ("do" ++ actName ++ "Action")
  methArgs
  [BlockStmt (IfThenElse
              (ExpName (Name [Ident "isAccessible"]))
              (StmtBlock (Block
                          [BlockStmt
                           (ExpStmt
                            (MethodInv
                             (PrimaryMethodCall
                              (MethodInv
                               (MethodCall (Name [Ident "runner",Ident$ "get"++actName]) [])) []
                              (Ident "trigger") [ExpName (Name [Ident "newVisual"])])))]))
              (StmtBlock (Block
                          [BlockStmt
                           (Throw (InstanceCreation []
                                   (ClassType [(Ident "RuntimeException",[])])
                                   [Lit (String$"Access forbidden for "++actName++" action")] Nothing))])))]
proxyCl :: String
        -> Maybe Type
        -> String
        -> [FormalParam]
        -> [BlockStmt] -> Decl
proxyCl proxyName srcTy pMethName methArgs proxyBody =
  MemberDecl
  (MemberClassDecl
   (ClassDecl [Protected,Final]
    (Ident proxyName) [] Nothing []
    (ClassBody [ MemberDecl
                 (ConstructorDecl [Private] [] (Ident proxyName) [] []
                  (ConstructorBody Nothing []))
               , methodDecl [Final,Private] Nothing
                 "setAccessible"
                 [FormalParam [] (PrimType BooleanT) False (VarId (Ident "isAccessible"))]
                 (Just (Block
                         [BlockStmt
                          (ExpStmt (Assign (FieldLhs (PrimaryFieldAccess This (Ident "isAccessible")))
                                    EqualA (ExpName (Name [Ident "isAccessible"]))))]))
               , MemberDecl (FieldDecl [Private]
                             (PrimType BooleanT)
                             [VarDecl (VarId (Ident "isAccessible"))
                              (Just (InitExp (Lit (Boolean False))))])
               , methodDecl [Final,Public]
                 srcTy
                 pMethName methArgs
                 (Just
                  (Block
                   proxyBody
                  ))])))

proxyOn proxyName vName =
   [LocalVars []
    (RefType (ClassRefType (ClassType [(Ident proxyName,[])])))
    [VarDecl (VarId (Ident vName)) (Just (InitExp (InstanceCreation [] (ClassType [(Ident proxyName,[])]) [] Nothing)))]
   ,BlockStmt
    (ExpStmt (MethodInv (MethodCall (Name [Ident vName,Ident "setAccessible"]) [Lit (Boolean True)])))]

proxyOff proxyName vName =
  [BlockStmt (ExpStmt (MethodInv (MethodCall (Name [Ident vName,Ident "setAccessible"]) [Lit (Boolean False)])))]


clsContext :: Maybe (PackageDecl)
           -> String  -- ^ class name
           -> [RefType] -- ^ implemented interfaces. depends on interaction contract
           -> RefType -- ^ output type of the context.
           -> [Decl] -- ^ the declarations (functions, variables) to be put in the body
           -> CompilationUnit -- ^ gives back full class.
clsContext pkg clname i ty methods =
  clsResource pkg clname
    -- extends Publisher of Ty:
    (Just (ClassRefType (ClassType [(Ident "Publisher",[ActualType ty])])))
    -- implements:
    i -- implements Context and perhaps subscriber
    methods -- these depend on the interaction contract etc



clsController :: Maybe (PackageDecl)
              -> String  -- ^ class name
              -> [RefType] -- ^ implemented interfaces. depends on interaction contract
              -> [Decl] -- ^ the functions, variables to be put in the body
              -> CompilationUnit -- ^ gives back full class.
clsController pkg clname i methods =
  clsResource pkg clname
    -- extends nothing
    Nothing
    i -- implements Controller and perhaps subscriber
    methods -- these depend on the interaction contract etc

clsResource :: Maybe (PackageDecl)
            -> String  -- ^ class name
            -> Maybe RefType -- ^ does the class extend anything?
            -> [RefType] -- ^ what does it implement?
            -> [Decl] -- ^ body of the class
            -> CompilationUnit -- ^ give back a complete class
clsResource pkg clname extends implements body =
  CompilationUnit pkg [] -- no imports
  [ClassTypeDecl $ ClassDecl publicAbstract
   (Ident$ "Abstract"++clname)
   [] -- not a generic class, hence no type parameters
   extends implements
   (ClassBody body)]

clsAction :: Maybe (PackageDecl)
          -> String  -- ^ class name
          -> RefType -- ^ input type of the action.
          -> CompilationUnit -- ^ gives back full class.
clsAction pkg clname ty =
  clsResource pkg clname
    Nothing -- does not extend anything
    [ClassRefType (ClassType [(Ident "Action",[ActualType ty])])] -- implements Action
    (actionMethod clname ty)

clsSource :: Maybe (PackageDecl)  -- ^ package name
          -> String  -- ^ class name
          -> RefType -- ^ output type of the source.
          -> CompilationUnit -- ^ gives back a full class.
clsSource pkg clname ty =
  clsResource pkg clname
   (Just -- all sources extend publisher, with their type as Generics param.
    (ClassRefType (ClassType [(Ident "Publisher", [ActualType ty])])))
   [ClassRefType  (ClassType [(Ident "Source",    [ActualType ty])])] -- implements Source interface.
   (sourceMethod clname ty)

publicFinal       = [Public,    Final   ]
publicAbstract    = [Public,    Abstract]
protectedAbstract = [Protected, Abstract]

  
sourceMethod :: String -- ^ name of source
             -> RefType -- ^ expected return type.
             -> [Decl]
sourceMethod nm ty =
  [ methodDecl protectedAbstract (Just$ RefType ty)
    ("get"++nm++"Value") [] Nothing
  , methodDecl [Public] (Just$RefType ty)
    "requireValue" []
    (Just (Block [BlockStmt (Return (Just (MethodInv (MethodCall (Name [Ident ("get"++nm++"Value")]) []))))]))
  ]
  
actionMethod :: String -- ^ name of source
             -> RefType -- ^ expected return type.
             -> [Decl]
actionMethod nm ty =
  [methodDecl protectedAbstract Nothing 
   ("do"++nm++"Action") (funcparams$[(ty, "value")]) Nothing
  ,methodDecl [Public] Nothing
   "trigger" (funcparams$[(ty, "value")])
   (Just (Block [BlockStmt (ExpStmt (MethodInv (MethodCall (Name [Ident ("do"++nm++"Action")])
                                                [ExpName (Name [Ident "value"])])))]))
  ]
                                
