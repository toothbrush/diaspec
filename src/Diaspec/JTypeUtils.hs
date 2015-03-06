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


pack n = packagify $ concat ["fr.diaspec.", n, ".generated"]
  where packagify s = Just (PackageDecl (Name (map Ident (split "." s))))

stmts = Just . Block


lVar ty nm value = LocalVars [] ty [VarDecl (VarId (Ident nm)) value]

proxyCl srcName proxyName srcTy  =
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
                            (Just (RefType srcTy))
                            (Ident "queryProxy") [] []
                            (MethodBody
                             (Just
                              (Block
                               [BlockStmt
                                (IfThen (ExpName (Name [Ident "isAccessible"]))
                                 (StmtBlock
                                  (Block
                                   [BlockStmt
                                    (Return (Just
                                             (MethodInv (PrimaryMethodCall (MethodInv (MethodCall
                                                                                       (Name [Ident "runner",Ident$"get"++srcName])
                                                                                       []))
                                                         []
                                                         (Ident$ "get"++srcName ++"Value") []))))])))
                               ,BlockStmt (Throw
                                           (InstanceCreation [] (ClassType [(Ident "RuntimeException",[])])
                                            [Lit (String$"Access forbidden for "++ srcName++ " source")] Nothing))]))))])))






