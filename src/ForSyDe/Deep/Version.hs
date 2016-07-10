-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Deep.Version
-- Copyright   :  (c) ES Group, KTH/ICT/ES 2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Version information for the current build
--
-----------------------------------------------------------------------------

module ForSyDe.Deep.Version (getVersion) where

import Paths_ForSyDe_Deep (version)
import Data.Version (showVersion)

import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Syntax
import System.Process (readProcess)
import Control.Exception (catch)
import System.IO.Error (IOError)

getVersion :: Q Exp
getVersion = do
        vstr <- runIO $ catch version_git version_cabal
        return (LitE $ StringL $ init vstr)

  where
    version_git :: IO String
    version_git = readProcess "git" ["describe", "--tags", "--dirty"] ""
    version_cabal :: IOError -> IO String
    version_cabal _ = return $ showVersion version
