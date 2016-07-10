-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe
-- Copyright   :  (c) ES Group (KTH) 2007-2013
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module is a wrapper for all the publicly usable types and
-- functions of ForSyDe's deep-embedded Domain Specific Language
-- (DSL). For the shallow-embedded DSL, please see "ForSyDe.Shallow".
--
-- 
-----------------------------------------------------------------------------
module ForSyDe.Deep 
(module ForSyDe.Deep.Ids,
 module ForSyDe.Deep.Signal,
 module ForSyDe.Deep.System,
 module ForSyDe.Deep.Process,
 module ForSyDe.Deep.Backend,
 module ForSyDe.Deep.Bit,
 module ForSyDe.Deep.AbsentExt,
 module ForSyDe.Deep.DFT,
 module ForSyDe.Deep.FIR,
 forsydeVersion) where

import ForSyDe.Deep.Ids
import ForSyDe.Deep.Signal (Signal)
import ForSyDe.Deep.Bit
import ForSyDe.Deep.Process
import ForSyDe.Deep.System
import ForSyDe.Deep.Backend
import ForSyDe.Deep.AbsentExt
import ForSyDe.Deep.DFT
import ForSyDe.Deep.FIR
import ForSyDe.Deep.Config (forsydeVersion)
