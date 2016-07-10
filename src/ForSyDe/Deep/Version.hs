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

module ForSyDe.Deep.Version (gitVersion) where

import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Syntax
import System.Process (readProcess)

gitVersion :: Q Exp
gitVersion = do
        vstr <- runIO $ readProcess "git" ["describe", "--tags", "--dirty"] ""
        return (LitE $ StringL $ init vstr)

