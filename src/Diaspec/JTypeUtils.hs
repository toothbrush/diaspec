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


methodDecl :: [Modifier] -- ^ scope modifiers, abstract, final
           -> Maybe Type -- ^ expected return type.
           -> String       -- ^ desired method name
           -> [FormalParam] -- ^ method arguments
           -> Maybe Block
           -> Decl -- ^ a method declaration.
methodDecl scope ret nm args bod = MemberDecl$ MethodDecl scope [] ret (Ident nm) args
                                   [] -- no exceptions
                                   (MethodBody bod)

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
    (ClassBody [MemberDecl
                (ConstructorDecl [Private] [] (Ident proxyName) [] []
                 (ConstructorBody Nothing []))
               ,MemberDecl
                (MethodDecl [Final,Private] [] Nothing
                 (Ident "setAccessible")
                 [FormalParam [] (PrimType BooleanT) False (VarId (Ident "isAccessible"))] []
                 (MethodBody
                  (Just (Block
                         [BlockStmt
                          (ExpStmt (Assign (FieldLhs (PrimaryFieldAccess This (Ident "isAccessible")))
                                    EqualA (ExpName (Name [Ident "isAccessible"]))))]))))
               ,MemberDecl (FieldDecl [Private]
                            (PrimType BooleanT)
                            [VarDecl (VarId (Ident "isAccessible"))
                             (Just (InitExp (Lit (Boolean False))))])
               ,MemberDecl (MethodDecl [Final,Public] []
                            srcTy
                            (Ident$ pMethName) methArgs []
                            (MethodBody
                             (Just
                              (Block
                               proxyBody
                               ))))])))

proxyOn proxyName vName =
   [LocalVars []
    (RefType (ClassRefType (ClassType [(Ident proxyName,[])])))
    [VarDecl (VarId (Ident vName)) (Just (InitExp (InstanceCreation [] (ClassType [(Ident proxyName,[])]) [] Nothing)))]
   ,BlockStmt
    (ExpStmt (MethodInv (MethodCall (Name [Ident vName,Ident "setAccessible"]) [Lit (Boolean True)])))]

proxyOff proxyName vName =
  [BlockStmt (ExpStmt (MethodInv (MethodCall (Name [Ident vName,Ident "setAccessible"]) [Lit (Boolean False)])))]


