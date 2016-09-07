{-# LANGUAGE TemplateHaskell #-}

-- A vecCounter, the simplest system with which to test netlist loops

module MapLambdaVector where

import ForSyDe.Deep
import Data.TypeLevel.Num hiding ((+))
import qualified Data.Param.FSVec as V
import Data.Int

vecLamCounter :: Signal (V.FSVec D4 Int32)
vecLamCounter  = sourceSY "vecCounterSource" add1 (V.copy d4 (0 :: Int32))
 where add1 = $(newProcFun [d| add1v :: (V.FSVec D4 Int32) -> (V.FSVec D4 Int32)
                               -- Only lambda functions which are enclosed in a
                               -- type signature are supported:
                               add1v v = V.map ((\a -> a+1) :: Int32 -> Int32) v |])

vecLamCounterSys :: SysDef (Signal (V.FSVec D4 Int32))
vecLamCounterSys = newSysDef vecLamCounter "vecLambdaCounterMapV" [] ["countVal"]

simLamVecCounter :: [(V.FSVec D4 Int32)]
simLamVecCounter = simulate vecLamCounterSys
