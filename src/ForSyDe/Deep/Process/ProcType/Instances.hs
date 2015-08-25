{-# LANGUAGE ScopedTypeVariables, FlexibleInstances,
             UndecidableInstances, TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Deep.System.SysFun.Instances
-- Copyright   :  (c) ES Group, KTH/ICT/ES 2007-2013
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  non-portable (Template Haskell)
--
-- This module provides the 'ProcType' instances (and 'Data',
-- 'Typeable' and 'Lift' when necessary) . It cannot be included in
-- "ForSyDe.Process.ProcType" directly due to a Template Haskell bug
-- which prevents Template Haskell from executing functions defined in
-- the same module: <http://hackage.haskell.org/trac/ghc/ticket/1800>
--
----------------------------------------------------------------------------- 
module ForSyDe.Deep.Process.ProcType.Instances where

import ForSyDe.Deep.Config (maxTupleSize)
import ForSyDe.Deep.Process.ProcType
import ForSyDe.Deep.AbsentExt

import Data.TypeLevel.Num.Sets (Nat, toInt)
import Data.Param.FSVec (FSVec, reallyUnsafeVector)


import Data.Data
import Control.Monad (liftM, liftM2, mzero)
import Text.ParserCombinators.ReadP
import Data.Set (empty, singleton)
import Language.Haskell.TH.Syntax (Lift(..), runIO)

-------------------------------------------------------------
-- Checking if a member of Data belongs to an enumerated type
-------------------------------------------------------------

-- | Phony type used to check if a data constructor is enumerate (has )
newtype IsConsEnum a = IsConsEnum {unIsConsEnum :: Bool}

-- | Tell if a member of "Data" belongs to an enumerated type
isConsEnum :: Data a => Constr -> IsConsEnum a
isConsEnum = gunfold  (\_ -> IsConsEnum False) (\_ -> IsConsEnum True) 

-- | Tell if a member of "Data" belongs to an enumerated type
--   and return its description.
getEnumAlgTy :: forall a . (Typeable a, Data a) => a -> Maybe EnumAlgTy
getEnumAlgTy a = case dataTypeRep dt of
  AlgRep cons -> do 
   strs <- mapM (\c -> toMaybe (unIsConsEnum (isConsEnum c :: IsConsEnum a)) 
                               (showConstr c)) cons 
   return (EnumAlgTy dn strs)
  _ -> Nothing
 where dt = dataTypeOf a
       tycon = typeRepTyCon.typeOf $ a
       modName = tyConModule tycon
       baseName = tyConName tycon
       dn = modName ++ "." ++ baseName
       toMaybe bool c = if bool then Just c else Nothing

-------------
-- Instances
-------------

instance {-# OVERLAPPABLE #-} (Lift a, Data a) => ProcType a where
 getEnums _ = maybe empty singleton (getEnumAlgTy (undefined :: a))
 -- We add parenthesis and try to use gread. 
 -- In addition, since gread is broken for unit (), we create or own parser
 readProcType = do skipSpaces
                   -- get all the input while no separator is found
                   str <- munch1 (\c -> not (elem c " ,()<>" )) 
                   gReadUnaryCons str
  where -- Generically read a unary constructor (possibly an integer, float etc .. )
        gReadUnaryCons :: forall a . Data a => String -> ReadP a
        gReadUnaryCons str = do 
              cons <- maybe mzero return $ readConstr (dataTypeOf (undefined :: a)) str
              fromConstrM (fail "readProcType: non-unary constructor found") cons



instance (Typeable s, Nat s, ProcType a) => ProcType (FSVec s a) where
 getEnums _ = getEnums (undefined :: a)
 readProcType = do
          skipSpaces  
          _ <- char '<'
          elems <- countSepBy (toInt (undefined :: s))
                              readProcType 
                              (skipSpaces >> char ',' >> skipSpaces)
          _ <- char '>'
          return (reallyUnsafeVector elems)
   where countSepBy n p sep = 
            if n == 0 
               then return []
               else liftM2 (:) p (sequence (replicate (n-1) (sep >> p))) 
  

instance ProcType a =>  ProcType (AbstExt a) where
 getEnums _ = getEnums (undefined :: a)
 readProcType = skipSpaces >> (absP <++ prstP)
   where absP = do _ <- string "Abst" 
                   return Abst
         prstP = do _ <- string "Prst"
                    skipSpaces
                    v <- readProcType
                    return $ Prst v

-- Tuple instances
$(let concatMapM f xs = liftM concat (mapM f xs) 
      msg = "Generating and compiling " ++ show (maxTupleSize -2) ++ 
            " tuple instances of " ++
            show ''ProcType ++  
            ", this might take some time ... \n"
  in runIO (putStrLn $ msg) >>
     concatMapM genTupInstances [2..maxTupleSize])



