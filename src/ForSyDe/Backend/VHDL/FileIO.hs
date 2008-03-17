-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Backend.VHDL.FileIO
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions working with files in the VHDL backend. 
--
-----------------------------------------------------------------------------
module ForSyDe.Backend.VHDL.FileIO where

import ForSyDe.Backend.VHDL.AST
import qualified ForSyDe.Backend.VHDL.Ppr as VHDLPpr (ppr)

import System.IO
import Text.PrettyPrint.HughesPJ

-- | Write a design file to a file in disk
writeDesignFile :: DesignFile -> FilePath -> IO ()
writeDesignFile df fp = do
 handle     <- openFile fp WriteMode
 hPutStr handle $ (render . VHDLPpr.ppr) df