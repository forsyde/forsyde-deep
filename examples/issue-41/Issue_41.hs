{-# LANGUAGE TemplateHaskell #-}
module Issue_41 where
import ForSyDe.Deep 
import Data.Int
import Data.TypeLevel
import Data.Param.FSVec

takeProcFun :: ProcFun(FSVec D2 (FSVec D2 Int32) -> FSVec D2 (FSVec D2 Int32))
takeProcFun = $(newProcFun
    [d| takeProcFun :: FSVec D2 (FSVec D2 Int32) -> FSVec D2 (FSVec D2 Int32) 
        takeProcFun x = x
    |])

takeProc :: Signal (FSVec D2 (FSVec D2 Int32)) -> Signal (FSVec D2 (FSVec D2 Int32)) 
takeProc = mapSY "proc1" takeProcFun

takeSysDef :: SysDef(Signal (FSVec D2 (FSVec D2 Int32)) -> Signal (FSVec D2 (FSVec D2 Int32)))
takeSysDef = newSysDef takeProc "mingzi" ["in"] ["out"]

x1 = (1 :: Int32) +> (2 :: Int32) +> empty
x2 = (4 :: Int32) +> (5 :: Int32) +> empty

y1 = x1 +> x2 +> empty
