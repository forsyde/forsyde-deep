{-# LANGUAGE TemplateHaskell #-}  
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.TH.LiftInstances
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides  'Lift' instances for all the AST-types defined
-- in "Language.Haskell.Syntax":
-- 'Guard' 'Strict', 'Callconv', 'Safety','Body', 'Con', 'FunDep', 'Foreign', 
-- 'Lit', 'Pat', 'Match', 'Stmt', 'Range', 'Clause', 'Type', 'Dec', 'Exp'
--
-- Furthermore it provides a 'Lift' instance of 'Ratio', 'Int8', 'Int16',
-- 'Int32',  
-- (essential for some of the other instantiations) and a function (metaLift)
-- which lifts an expression twice, obtaing its meta AST (the AST of the AST)
-- 
-----------------------------------------------------------------------------
module Language.Haskell.TH.LiftInstances (metaLift) where

import Language.Haskell.TH.Lift (deriveLift)

import Language.Haskell.TH.Syntax
 (Guard,
  Strict,
  Callconv,
  Safety,
  Body, 
  Con, 
  FunDep, 
  Foreign, 
  Lit(IntegerL), 
  Pat, 
  Match, 
  Stmt, 
  Range, 
  Clause, 
  Type, 
  Dec, 
  Exp(LitE),
  Q,
  Lift(..),
  Pred,
  TyVarBndr,
  FamFlavour,
  Pragma,
  AnnLookup,
  AnnTarget,
  Fixity,
  FixityDirection,
  Inline,
  Module,
  ModuleInfo,
  Phases,
  Role,
  RuleBndr,
  RuleMatch,
  TyLit,
  TySynEqn
  )

import Data.Ratio (Ratio)
import Data.Int (Int8, Int16, Int32, Int64)
import GHC.Word (Word8)

$(mapM deriveLift 
      [''Ratio,
       ''Guard,
       ''Strict,
       ''Callconv,
       ''Safety,
       ''Body, 
       ''Con, 
       ''FunDep, 
       ''Foreign, 
       ''Lit, 
       ''Pat, 
       ''Match, 
       ''Stmt, 
       ''Range, 
       ''Clause, 
       ''Type, 
       ''Dec, 
       ''Exp,
       ''TyVarBndr,
       ''FamFlavour,
       ''Pragma,

       ''AnnLookup,
       ''AnnTarget,
       ''Fixity,
       ''FixityDirection,
       ''Inline,
       ''Module,
       ''ModuleInfo,
       ''Phases,
       ''Role,
       ''RuleBndr,
       ''RuleMatch,
       ''TyLit,
       ''TySynEqn
       ])
       
instance Lift Int64 where
  lift x = return (LitE (IntegerL (fromIntegral x)))


instance Lift Int32 where
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Int16 where
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Int8 where
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Word8 where
  lift x = return (LitE (IntegerL (fromIntegral x)))


-- | lift twice, getting the meta AST (the AST of the AST)
metaLift :: Lift a => a -> Q Exp
metaLift exp = do expAST <- lift exp
                  lift expAST

