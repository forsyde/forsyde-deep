{-# LANGUAGE CPP, TemplateHaskell, Rank2Types #-}
module ForSyDe.Deep.Process.Desugar (desugarTransform) where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH
import Control.Monad.State.Lazy

import Data.Generics
import qualified Data.Param.FSVec as V

{-
desugarTransform :: [Dec] -> Q [Dec]
desugarTransform = return
-}

desugarTransform :: [Dec] -> Q [Dec]
--desugarTransform decs = foldAccDecs (mkM extractTypedLambda) decs
desugarTransform decs = (return decs) 
                      =+=> extractTypedLambda
                      =+=> extractInfixOpSection
                      =+=> specializeHof
                      =+=> deleteSigE
#ifdef DEVELOPER
                      >>= dumpTree
  where
    dumpTree :: [Dec] -> Q [Dec]
    dumpTree ts = do _ <- runIO $ mapM (print.ppr) ts
                     return ts
#endif

-- Apply declaration-accumulating transformation
(=+=>) :: Typeable a => Q [Dec] -> (a -> DecAccM a) -> Q [Dec]
decs =+=> f = decs >>= (foldAccDecs $ mkM f)

-- Extract all occurences of lambda expressions within type signatures 
--    ... ((\...  -> ...) :: ...) ...
-- Create functions declarations for them and replace the lambda expression
-- with a reference to the declaration:
--    ... lambda0 ...
--    where
--      lambda0 :: ...
--      lambda0 ... = ...
extractTypedLambda :: Exp -> DecAccM Exp
extractTypedLambda (SigE (LamE patterns lamBody) ty) = do
    name <- lift $ newName "lambda"
    let body = NormalB lamBody
        signature = (SigD name ty)
        function  = (FunD name [(Clause patterns body [])])
    modify $ \declState -> signature:function:declState
    return (VarE name)
extractTypedLambda e = return e

-- Make a function out of a suitably annotated operator section
--    ... (+3)::Int32->Int32 ...
--  is transformed to
--    ... infix_section_0 ...
--    where infix_section_0 a = a+3
extractInfixOpSection :: Exp -> DecAccM Exp
extractInfixOpSection (SigE exp@(InfixE _ _ _) ty) = do
    name    <- lift.newName $ "infix_section"
    argname <- lift.newName $ "a"
    let body      = NormalB $ everywhere (mkT $ insertArgument argname) exp
        patterns  = [VarP argname]
        signature = SigD name ty
        function  = FunD name [(Clause patterns body [])]
    modify $ \declState -> signature:function:declState
    return (VarE name)
  where
    insertArgument :: Name -> Exp -> Exp
    insertArgument argn (InfixE Nothing op r) = InfixE (Just $ VarE argn) op r
    insertArgument argn (InfixE l op Nothing) = InfixE l op (Just $ VarE argn)
    insertArgument _ e = e
extractInfixOpSection e = return e


hofNames :: [TH.Name]
hofNames = [
            'V.foldr,
            'V.foldl,
            'V.map,
            'V.zipWith,
            'V.zipWith3
            ]

isHigherOrderFunctionName :: TH.Name -> Bool
isHigherOrderFunctionName n = elem n hofNames

-- specializing extraction:
-- find all hof applications: (AppE (VarE hofName) (VarE fname))
--      build specialized function definition in normal form
--      collect specialized declarations in state of DecAccM
--      replace application with call to specialized function
specializeHof :: Exp -> DecAccM Exp
-- arity=2
specializeHof (SigE (AppE hofapp@(AppE (VarE hofName) (VarE argFunName)) arg1@(SigE _ argtype)) rettype)
   | isHigherOrderFunctionName hofName = do 
        name    <- lift.newName $ (nameBase hofName)++"_"++(nameBase argFunName)
        argname <- lift.newName $ "v"

        let body      = NormalB $ AppE hofapp (VarE argname)
            patterns  = [VarP argname]
            signature = (SigD name (AppT (AppT ArrowT argtype) rettype))
            function  = (FunD name [(Clause patterns body [])])
        modify $ \declState -> declState++[signature,function]
        return (SigE (AppE (VarE name) arg1) rettype)
specializeHof e = return e

-- Delete all the type signature expressions as they are not recognized during
-- translation
deleteSigE :: Exp -> DecAccM Exp
deleteSigE (SigE e _) = return e
deleteSigE e = return e

type DecAccM t = StateT [Dec] Q t

-- stateful fold over the tree. Applies f everywhere within the tree,
-- collecting additional declarations during traversal, and adding them to the
-- corresponding scope
foldAccDecs :: GenericM DecAccM -> [Dec] -> Q [Dec]
foldAccDecs transform decs = mapM apply decs
 where
  apply :: Dec -> Q Dec
  apply (FunD name [Clause pat body decls]) = do
    (newBody,newDecls) <- runStateT (everywhereM transform body) []
    transfDecls <- foldAccDecs transform decls
    return $ FunD name [Clause pat newBody (transfDecls++newDecls)]
  apply (ValD pat body decls) = do
    (newBody,newDecls) <- runStateT (everywhereM transform body) []
    transfDecls <- foldAccDecs transform decls
    return $ ValD pat newBody (transfDecls++newDecls)
  apply d = return d
