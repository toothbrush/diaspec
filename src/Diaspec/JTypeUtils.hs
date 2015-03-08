module Diaspec.JTypeUtils where

import Language.Java.Syntax
import Data.String.Utils (split)
import Data.Char (toLower)

wrapMaybe :: RefType -> RefType
wrapMaybe t = classRef [(Ident "Maybe",[ActualType t])]

methCall :: [String] -> [Argument] -> Exp
methCall mnames args = MethodInv (MethodCall idents args)
        where idents = Name (map Ident mnames)
  
methInv :: [String] -> [Argument] -> Stmt
methInv m a = ExpStmt (methCall m a) -- todo uncurry?

methInvInit :: [String] -> [Argument] -> VarInit
methInvInit mname arg = InitExp (methCall mname arg)

funcparams :: [(RefType, String)] -> [FormalParam]
funcparams = map (\(t,n) -> FormalParam [] (RefType t) False (VarId (Ident n)))


absRunCls :: RefType
absRunCls = classRef [(Ident "AbstractRunner",[])]


runnerInit :: Bool -- ^ should it be Final?
           -> [Decl]
runnerInit b    = [ MemberDecl (FieldDecl (runnerVarModifs b) (RefType absRunCls)
                                [VarDecl (VarId (Ident "runner")) Nothing])
                  , methodDecl (runnerMethModifs b) Nothing
                    "init"
                    (funcparams [(absRunCls, "runner")])
                    (Just (Block [BlockStmt
                                  (assign (FieldLhs (PrimaryFieldAccess This (Ident "runner")))
                                   (varAccess "runner"))]))
                  ]
  where runnerMethModifs True  = [Final, Protected]
        runnerMethModifs False = [Protected]
        runnerVarModifs  True  = [Private]
        runnerVarModifs  False = [Protected]

varAccess :: String -> Exp
varAccess n = ExpName (Name [Ident n])

classRef nms = ClassRefType (ClassType nms)

pack :: String -> Maybe PackageDecl
pack n = packagify $ concat ["fr.diaspec.", n, ".generated"]
  where packagify s = Just (PackageDecl (Name (map Ident (split "." s))))

stmts :: [BlockStmt] -> Maybe Block
stmts = Just . Block

stBlock l = StmtBlock (Block (map BlockStmt l))
  
lVar :: Type -> String -> Maybe VarInit -> BlockStmt
lVar ty nm value = LocalVars [] ty [VarDecl (VarId (Ident nm)) value]

proxyClGet :: String -> String -> Maybe Type -> Decl
proxyClGet srcName proxyName srcTy =
  proxyCl proxyName srcTy ("query" ++ srcName ++ "Value")
  []
  [BlockStmt (IfThenElse
              (ExpName (Name [Ident "isAccessible"]))
              (stBlock
               [Return (Just
                        (MethodInv
                         (PrimaryMethodCall
                          (methCall ["runner","get"++srcName++"Instance"] [])
                          []
                          (Ident "requireValue") [])))])
              (stBlock
               [Throw (InstanceCreation []
                         (ClassType [(Ident "RuntimeException",[])])
                         [Lit (String$"Access forbidden for "++srcName++" source")] Nothing)]))]

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
              (stBlock
               [ExpStmt
                 (MethodInv
                  (PrimaryMethodCall
                   (methCall
                    ["runner","get"++actName++"Instance"] []) []
                   -- todo shoudl probably parameterise over "value"
                   (Ident "trigger") [varAccess "value"]))])
              (stBlock
               [Throw (InstanceCreation []
                       (ClassType [(Ident "RuntimeException",[])])
                       [Lit (String$"Access forbidden for "++actName++" action")] Nothing)]))]


assign :: Lhs -> Exp -> Stmt
assign l r = ExpStmt (Assign l EqualA r)

  
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
                          (assign (FieldLhs (PrimaryFieldAccess This (Ident "isAccessible")))
                           (ExpName (Name [Ident "isAccessible"])))]))
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
    (RefType (classRef [(Ident proxyName,[])]))
    [VarDecl (VarId (Ident vName)) (Just (InitExp (InstanceCreation [] (ClassType [(Ident proxyName,[])]) [] Nothing)))]
   ,BlockStmt
    (methInv [vName,"setAccessible"] [Lit (Boolean True)])]

proxyOff proxyName vName =
  [BlockStmt (methInv [vName,"setAccessible"] [Lit (Boolean False)])]


clsContext :: Maybe PackageDecl
           -> String  -- ^ class name
           -> [RefType] -- ^ implemented interfaces. depends on interaction contract
           -> RefType -- ^ output type of the context.
           -> [Decl] -- ^ the declarations (functions, variables) to be put in the body
           -> CompilationUnit -- ^ gives back full class.
clsContext pkg clname i ty =
  clsResource pkg clname
    -- extends Publisher of Ty:
    (Just (classRef [(Ident "Publisher",[ActualType ty])]))
    i -- implements Context and perhaps subscriber



clsController :: Maybe PackageDecl
              -> String  -- ^ class name
              -> [RefType] -- ^ implemented interfaces. depends on interaction contract
              -> [Decl] -- ^ the functions, variables to be put in the body
              -> CompilationUnit -- ^ gives back full class.
clsController pkg clname = 
  clsResource pkg clname
    -- extends nothing
    Nothing

clsResource :: Maybe PackageDecl
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

clsAction :: Maybe PackageDecl
          -> String  -- ^ class name
          -> RefType -- ^ input type of the action.
          -> CompilationUnit -- ^ gives back full class.
clsAction pkg clname ty =
  clsResource pkg clname
    Nothing -- does not extend anything
    [classRef [(Ident "Action",[ActualType ty])]] -- implements Action
    (actionMethod clname ty)

clsSource :: Maybe PackageDecl  -- ^ package name
          -> String  -- ^ class name
          -> RefType -- ^ output type of the source.
          -> CompilationUnit -- ^ gives back a full class.
clsSource pkg clname ty =
  clsResource pkg clname
   (Just -- all sources extend publisher, with their type as Generics param.
    (classRef [(Ident "Publisher", [ActualType ty])]))
   [classRef  [(Ident "Source",    [ActualType ty])]] -- implements Source interface.
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
    (Just (Block [BlockStmt
                  (Return
                   (Just
                    (methCall ["get"++nm++"Value"] [])))]))
  ]
  
actionMethod :: String -- ^ name of source
             -> RefType -- ^ expected return type.
             -> [Decl]
actionMethod nm ty =
  [methodDecl protectedAbstract Nothing 
   ("do"++nm++"Action") (funcparams [(ty, "value")]) Nothing
  ,methodDecl [Public] Nothing
   "trigger" (funcparams [(ty, "value")])
   (Just (Block [BlockStmt
                 (methInv ["do"++nm++"Action"]
                  [varAccess "value"])]))
  ]
                                

clsRunner pkg = clsResource pkg "Runner"
                (Just -- extends CommonRuncode
                 (classRef [(Ident "CommonRuncode", [])]))
                [] -- implements nothing

initFunc body = methodDecl
    [Annotation
     MarkerAnnotation {annName = Name [Ident "Override"]}
    ,Protected] Nothing
    "init" []
    (Just (Block body))

fieldDecl nm v init = MemberDecl
   (FieldDecl (modifs init)
    (RefType (classRef [(Ident$ "Abstract"++nm,[])]))
    [VarDecl (VarId (Ident v))
     (val init)])
   where modifs True  = [Private, Static, Final]
         modifs False = [Private]
         val    True  = Just (InitExp (InstanceCreation []
                                       (ClassType [(Ident nm,[])]) [] Nothing))
         val    False = Nothing

deployMethod nm v init = 
  methodDecl (modifs init)
   (Just (RefType
          (classRef [(Ident$"Abstract"++nm,[])])))
   ("get"++nm++"Instance") []
   (impl init)
  where impl True  = Just (Block
                          [BlockStmt (Return (Just
                                              (varAccess v)))])
        impl False = Nothing
        modifs True  = [Public]
        modifs False = [Public, Abstract]
  
addtoList list what = BlockStmt
            (methInv [list, "add"]
             [varAccess what])

initComponent nm v =
  [ BlockStmt (assign (NameLhs (Name [Ident v]))
               (methCall ["get"++nm++"Instance"] []))
  , BlockStmt (methInv [v, "init"] [This])]

subscribesTo recv pub = [BlockStmt (methInv [pub, "addSubscriber"]
                                [varAccess recv])]

instanceVar = (++ "_") . map toLower
