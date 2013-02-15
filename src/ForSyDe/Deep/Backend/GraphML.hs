-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Deep.Backend.GraphML
-- Copyright   :  (c) ES Group, KTH/ICT/ES 2013
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides the GraphML backend of ForSyDe's embedded compiler
--
-----------------------------------------------------------------------------
-- FIXME: factorize code shared between backends, and create a common interface for
--        backends (maybe based on a MPTC)

module ForSyDe.Deep.Backend.GraphML
 (writeGraphML, 
  writeGraphMLOps, 
  GraphMLOps(..),
  GraphMLDebugLevel(..),
  GraphMLRecursivity(..),
  defaultGraphMLOps) where

import Control.Monad.State (evalStateT)
import ForSyDe.Deep.ForSyDeErr
import ForSyDe.Deep.OSharing (readURef)
import ForSyDe.Deep.System.SysDef
import ForSyDe.Deep.Backend.GraphML.Traverse

-- | Given a System Definition whose name is A generate @A.graphml@ in current 
--   working directory using the default compilation options.
writeGraphML :: SysDef a -> IO ()
writeGraphML = writeGraphMLOps defaultGraphMLOps 

-- | 'writeGraphML'-alternative which allows setting GraphML compilation 
--   options.
writeGraphMLOps :: GraphMLOps -> SysDef a -> IO ()
writeGraphMLOps ops sysDef = do
  -- initiate the compilation State
  let s = initGraphMLTravST $ (readURef.unPrimSysDef.unSysDef) sysDef
  -- Translate the code
  res <- runErrorT $ evalStateT  (setGraphMLOps ops >> writeGraphMLM) s
  -- Check if the  compilation went well and print an error in case it didn't
  either printGraphMLError return res
