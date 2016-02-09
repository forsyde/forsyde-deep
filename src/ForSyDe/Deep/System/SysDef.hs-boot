{-# LANGUAGE RoleAnnotations #-}
-- SysDef.hs-boot: GHC bootstrapping module for Netlist.hs
-- (it breaks the recursive import loop with Netlist.hs)
-- See "How to compile mutually recursive modules" in GHC's manual for details

module ForSyDe.Deep.System.SysDef where

import ForSyDe.Deep.OSharing
import ForSyDe.Deep.Ids
import Data.Typeable.FSDTypeRepLib

type Iface = [(PortId, FSDTypeRep)]

type role SysDef phantom
newtype SysDef a = SysDef {unSysDef :: PrimSysDef}

newtype PrimSysDef = PrimSysDef {unPrimSysDef :: URef SysDefVal}

data SysDefVal 

oIface :: SysDefVal -> Iface
