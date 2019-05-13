{-# LANGUAGE TemplateHaskell,CPP, DeriveLift #-}  
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
-- in "Language.Haskell.Syntax"
--
-- Furthermore it provides a function (metaLift) which lifts an expression
-- twice, obtaing its meta AST (the AST of the AST)
-- 
-----------------------------------------------------------------------------
module Language.Haskell.TH.LiftInstances (metaLift) where

import Language.Haskell.TH.Lift (deriveLift)

import Language.Haskell.TH.Syntax
 (Guard,
#if __GLASGOW_HASKELL__ >= 800
  -- Strict was replaced and became a synonym for Bang in GHC 8.0.1
  Bang,
  Overlap,
  SourceUnpackedness,
  SourceStrictness,
  DecidedStrictness,
  TypeFamilyHead,
  InjectivityAnn,
  FamilyResultSig,
#else
  -- This is only for backwards compatibility with ghc 7.10.
  Strict,
#endif
  Callconv,
  Safety,
  Body, 
  Con, 
  FunDep, 
  Foreign, 
  Lit, 
  Pat, 
  Match, 
  Stmt, 
  Range, 
  Clause, 
  Type, 
  Dec(..), 
  Exp,
  Q,
  Lift(..),
  TyVarBndr,
#if __GLASGOW_HASKELL__ < 804
  FamFlavour,
-- #else
--   OpenTypeFamilyD,
#endif
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
  TySynEqn,
---------------------------
  DerivClause(..),
  DerivStrategy(..),
  PatSynArgs(..),
  PatSynDir(..)
  )

-- instance Lift DerivClause where lift = pure
-- instance Lift DerivStrategy where lift = pure
-- instance Lift PatSynArgs where lift = pure
-- instance Lift PatSynDir where lift = pure


$(mapM deriveLift 
      [''Guard,
#if __GLASGOW_HASKELL__ >= 800
       -- Strict was replaced and became a synonym (for which we don't need any
       -- lift instances) for Bang in GHC 8.0.1
       ''Bang,
       ''Overlap,
       ''SourceUnpackedness,
       ''SourceStrictness,
       ''DecidedStrictness,
       ''TypeFamilyHead,
       ''InjectivityAnn,
       ''FamilyResultSig,
#else
       -- This is only for backwards compatibility with ghc 7.10.
       ''Strict,
#endif
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
#if __GLASGOW_HASKELL__ < 804
       ''FamFlavour,
-- #else
--        ''DataFamilyD,
#endif
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
       ''TySynEqn,
---------------------------------
       ''DerivClause,
       ''DerivStrategy,
       ''PatSynArgs,
       ''PatSynDir
       ])
       
-- | lift twice, getting the meta AST (the AST of the AST)
metaLift :: Lift a => a -> Q Exp
metaLift exp = do expAST <- lift exp
                  lift expAST
