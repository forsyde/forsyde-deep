-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Deep.Process
-- Copyright   :  (c) ES Group, KTH/ICT/ES 2007-2013
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Publicly usable functions to create primitive processes. (Reexports 
--  "ForSyDe.Process.SynchProc")
-- 
-----------------------------------------------------------------------------
module ForSyDe.Deep.Process 
 (ProcFun, newProcFun, defArgVal, defArgPF,
  ProcType,
  module ForSyDe.Deep.Process.SynchProc) where

import ForSyDe.Deep.Process.ProcFun (ProcFun, newProcFun, defArgVal, defArgPF)

import ForSyDe.Deep.Process.SynchProc 
import ForSyDe.Deep.Process.ProcType (ProcType)
import ForSyDe.Deep.Process.ProcType.Instances ()
