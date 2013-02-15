-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Deep.Backend.GraphML.FileIO
-- Copyright   :  (c) ES Group, KTH/ICT/ES 2007-2013
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions working with files in the GraphML backend. 
--
-----------------------------------------------------------------------------
module ForSyDe.Deep.Backend.GraphML.FileIO where

import ForSyDe.Deep.Backend.GraphML.AST
import ForSyDe.Deep.Backend.GraphML.Ppr(YFilesMarkup, pprGraphWithHeaders)

import System.IO
import Text.PrettyPrint.HughesPJ

-- | Write a design file to a file in disk
writeGraph :: YFilesMarkup -> GraphMLGraph -> FilePath -> IO ()
writeGraph yFiles graph fp = do
 handle     <- openFile fp WriteMode
 hPutStr handle $ (render . pprGraphWithHeaders yFiles) graph
 hClose handle
