{-# LANGUAGE TemplateHaskell #-} 
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Deep.Process.ProcFun
-- Copyright   :  (c) ES Group, KTH/ICT/ES 2007-2013
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  non-portable (Template Haskell)
--
-- This module provides a type ('ProcFun') to store the functions  passed
-- to process constructors.
--
----------------------------------------------------------------------------- 
module ForSyDe.Deep.Process.ProcFun 
 (ProcFun(..),
  ProcFunAST(..),
  newProcFun, 
  defArgVal,
  defArgPF,
  TypedProcFun(..),
  TypedProcFunAST(..),
  procFun2Dyn,
  contProcFun2Dyn,
) where

import ForSyDe.Deep.Process.ProcType
import ForSyDe.Deep.Process.Desugar (desugarTransform)
import ForSyDe.Deep.Process.ProcVal (ProcValAST, mkProcValAST)
import ForSyDe.Deep.ForSyDeErr

import Language.Haskell.TH hiding (Loc)
import Language.Haskell.TH.Syntax hiding (Loc)
import Language.Haskell.TH.LiftInstances ()
import Data.Dynamic
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Typeable.FSDTypeRepLib (FSDTypeRep, fsdTy)

-----------
-- ProcFun
-----------

-- | A Process Function 
data ProcFun a = 
   ProcFun {val  :: a,          -- ^ Value of the function
            pfloc :: Loc, -- ^ where was it created
            ast  :: ProcFunAST} -- ^ AST of the function



-- | A process Function AST
data ProcFunAST = 
  ProcFunAST {name  :: Name,     -- ^ Function Name 
                                    -- (FIXME: maybe just a String?) 
              cls   :: [Clause], -- ^ Function clauses 
              pars  :: [DefArg]} -- ^ Default parameters


-- | A process Function default argument
--   Either a process function AST or a value AST
data DefArg = FunAST ProcFunAST | ValAST ProcValAST



-- | Sets a default value for an argument of the process function
defArgVal :: (Lift a, ProcType a) => ProcFun (a -> b) -> a -> ProcFun b
-- FIXME: inneficient, use a queue data structure
defArgVal pf v = 
   pf{ ast = astPF {pars = ((pars astPF) ++ [ValAST (mkProcValAST v)])}, 
       val = (val pf) v}                    
  where astPF = ast pf

-- | Sets a default value for an argument of the process function 
--   when the argument is a process function itself
defArgPF :: ProcFun (a -> b) -> ProcFun a -> ProcFun b
defArgPF pf v = pf{ ast = astPF {pars = ((pars astPF) ++ [FunAST (ast v)])}, 
                    val = (val pf) (val v)}                    
  where astPF = ast pf 
                   
-- | Template Haskell constructor for 'ProcFun', here is an example on how to use it
--
-- @
--  plus1Fun :: ProcFun (Int -> Int)
--  plus1Fun = $(newProcFun [d| plus1 :: Int -> Int
--                              plus1 n = n + 1     |])
-- @
newProcFun :: Q [Dec] -> ExpQ
newProcFun fDecQs = do 
      fDecsRaw <- fDecQs   
      fDecs <- desugarTransform fDecsRaw
      -- Check for the declarations to be correct
      (name, cls) <- recover (currErr $ IncorrProcFunDecs fDecs) 
                             (checkDecs fDecs)
      -- Generate the main expression
      loc <- qLocation
      let errInfo = loc_module loc in do
      exp <-  [| let  fName    = name
                      fClauses = cls 
                 in ProcFun $(varE name)
                            errInfo 
                            (ProcFunAST fName fClauses []) |]
      -- Add the function declarations to the expression
      return $ LetE fDecs exp  
 where currErr = qError "newProcFun"

----------------
-- TypedProcFun
----------------


-- | A ProcFun bundled with its type representation. This type is not
--   exported to the end user. Only used internally.
data TypedProcFun a =    
   TypedProcFun {tval   :: a,          -- ^ Value of the function
                 tpfloc :: Loc,
                 tast   :: TypedProcFunAST} -- ^ AST of the function


-- | A ProcFunAST bundled with its type representation:
--   Why a TypeRep and not the Type provided by Template Haskell?
--    We could use the type signature provided by TH but ...
--     1) We don't want to force the user to provide a signature
--     2) We don't want to handle polymorphic types. We just want the
--        monomorphic type used by the process using the procfun.
--   Why not just including the 'TypeRep' in ProcFunAST?
--     We need the context of the process to know what monomorphic types are 
--     going to be used. Thus it is imposible to guess the TypeRep within
--     the code of newProcFun.
data TypedProcFunAST = 
     TypedProcFunAST {tptyp   :: FSDTypeRep,     -- function type
                      tpEnums :: Set EnumAlgTy,  -- enumerated types associated 
                                                 -- with the function
                      tpast   :: ProcFunAST}

-- | transform a ProcFun into a Dynamic TypedProcFun
procFun2Dyn :: Typeable a => Set EnumAlgTy -> ProcFun a -> TypedProcFun Dynamic
procFun2Dyn s (ProcFun v l a) = 
  TypedProcFun (toDyn v) l (TypedProcFunAST (fsdTy (typeOf v)) s a)

-- FIXME: probably not needed
-- | tranform the arguments and return value of
--   a ProcFun to dynamic
contProcFun2Dyn :: (Typeable1 container,
                    Typeable b,
                    Functor container, 
                    Typeable a) =>
                   Set EnumAlgTy ->
                   ProcFun (container a -> b) -> 
                   TypedProcFun (container Dynamic -> Dynamic)
contProcFun2Dyn s (ProcFun v l a) = 
     TypedProcFun (fmapDyn v) l (TypedProcFunAST (fsdTy (typeOf v)) s a)
       where  fmapDyn f cont = toDyn (f (fmap (fromJust.fromDynamic) cont)) 


----------------------------
-- Internal Helper Functions
----------------------------

-- | Check the decarations passed to newProcFun to be correct
checkDecs :: [Dec] -> Q (Name, [Clause])
checkDecs [FunD name2 cls] = return (name2, cls) 
-- in case a signature is provided
checkDecs [SigD name1 _, FunD name2 cls] | name1 == name2 = 
  return (name1, cls) 
checkDecs _                  = qGiveUp name
  where name = "ForSyDe.Process.ProcFun.checkDecs"
