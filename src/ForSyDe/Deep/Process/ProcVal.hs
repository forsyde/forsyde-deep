{-# LANGUAGE ScopedTypeVariables #-}
----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Deep.Process.ProcVal
-- Copyright   :  (c) ES Group, KTH/ICT/ES 2007-2013
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  non-portable (Template Haskell)
--
-- This module provides a type ('ProcVal') to store the value arguments passed
-- to process constructors.
--
----------------------------------------------------------------------------- 
module ForSyDe.Deep.Process.ProcVal where


import ForSyDe.Deep.Process.ProcType

import Data.Typeable (typeRep)
import Data.Dynamic (toDyn, Dynamic)
import Data.Set
import Data.Proxy
import Language.Haskell.TH (Exp, runQ)
import Language.Haskell.TH.Syntax (Lift(..))
import System.IO.Unsafe (unsafePerformIO)
import Data.Typeable.FSDTypeRepLib (fsdTy, FSDTypeRep)


data ProcVal = ProcVal 
                  {dyn     :: Dynamic,    --  Dynamic value 
                   valAST  :: ProcValAST} --  its AST

data ProcValAST = ProcValAST
                    {expVal   :: Exp,           -- Its AST representation
                     expTyp   :: FSDTypeRep,       -- Type of the expression 
                     expEnums :: Set EnumAlgTy} -- Enumerated types associated
                                                -- with the expression

-- | 'ProcVal' constructor
mkProcVal :: (Lift a, ProcType a) => a -> ProcVal
-- FIXME: would unsafePerformIO cause any harm to get the exp out of the
--        Q monad in this context?
mkProcVal val = ProcVal (toDyn val) (mkProcValAST val) 

mkProcValAST :: (Lift a, ProcType a) => a -> ProcValAST 
-- FIMXE: the unsafePerformIO won't be needed once the Data a => Lift a
--        instance is created
mkProcValAST (val :: x) = ProcValAST (unsafePerformIO.runQ.lift $ val) 
                                     (fsdTy.typeRep $ (Proxy :: Proxy x))
                                     (getEnums val)
