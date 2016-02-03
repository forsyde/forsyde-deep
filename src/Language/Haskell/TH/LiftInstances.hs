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
  Strict,
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
  Dec, 
  Exp,
  Q,
  Lift(..),
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

$(mapM deriveLift 
      [''Guard,
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
       
-- | lift twice, getting the meta AST (the AST of the AST)
metaLift :: Lift a => a -> Q Exp
metaLift exp = do expAST <- lift exp
                  lift expAST

