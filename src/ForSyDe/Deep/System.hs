-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Deep.System
-- Copyright   :  (c) ES Group, KTH/ICT/ES 2007-2013
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  non-portable (Template Haskell)
--
-- This module provides publicly usable functions to build a system definition
-- and instantiate it.
-- 
-----------------------------------------------------------------------------
module ForSyDe.Deep.System  
(SysDef, newSysDef, newSysDefTH, newSysDefTHName,
 SysFun, SysFunToSimFun, SysFunToIOSimFun,
 instantiate)
where

import ForSyDe.Deep.System.SysDef (SysDef, newSysDef, newSysDefTH, newSysDefTHName)
import ForSyDe.Deep.System.SysFun (SysFun, SysFunToSimFun, SysFunToIOSimFun)
import ForSyDe.Deep.System.SysFun.Instances ()
import ForSyDe.Deep.System.Instantiate (instantiate)
