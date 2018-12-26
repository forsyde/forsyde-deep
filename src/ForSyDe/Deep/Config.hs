{-# LANGUAGE TemplateHaskell, CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Deep.Config
-- Copyright   :  (c) ES Group, KTH/ICT/ES 2007-2013
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Configuration values of ForSyDe
--
-----------------------------------------------------------------------------
module ForSyDe.Deep.Config (forsydeVersion,
                            maxTupleSize,
                            module Paths_forsyde_deep) where

import Paths_forsyde_deep
import ForSyDe.Deep.Version (getVersion)

#ifdef DEVELOPER
maxTupleSize :: Int
maxTupleSize = 8
#else
import GHC.Exts (maxTupleSize)
#endif

forsydeVersion = $(getVersion)
