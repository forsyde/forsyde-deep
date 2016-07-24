{-# LANGUAGE TemplateHaskell #-}

-- A counter, the simplest system with which to test netlist loops

module MapVector where

import ForSyDe.Deep
import Data.TypeLevel.Num hiding ((+))
import qualified Data.Param.FSVec as V
import Data.Int

counter :: Signal (V.FSVec D4 Int32)
counter  = sourceSY "counterSource" add1 (V.copy d4 (0 :: Int32))
 where add1 = $(newProcFun [d| add1v :: (V.FSVec D4 Int32) -> (V.FSVec D4 Int32)
                               -- The higher order vector functions can only be
                               -- translated directly when they are the sole
                               -- expression of a function body:
                               add1v v = V.map add1 v
                                 where
                                     add1 :: Int32 -> Int32
                                     add1 a = a+1 |])

counterSys :: SysDef (Signal (V.FSVec D4 Int32))
counterSys = newSysDef counter "counterMapV" [] ["countVal"]

simCounter :: [(V.FSVec D4 Int32)]
simCounter = simulate counterSys
