{-# LANGUAGE TemplateHaskell #-}

-- A counter, the simplest system with which to test netlist loops

module MapVector where

import ForSyDe.Deep
import Data.TypeLevel.Num hiding ((+))
import qualified Data.Param.FSVec as V
import Data.Param.FSVec ((+>), empty)
import Data.Int

counter :: Signal (V.FSVec D4 Int32) -> Signal Int32
counter  = mapSY "counterSource" add1
 where add1 = $(newProcFun [d| add1v :: (V.FSVec D4 Int32) -> Int32
                               add1v v = foldladd1 0 v
                                 where
                                     add1 :: Int32 -> Int32 -> Int32
                                     add1 a b = a+b+1 
                                     -- The higher order vector functions can only be
                                     -- translated directly when they are the sole
                                     -- expression of a function body:
                                     foldladd1 :: Int32 -> (V.FSVec D4 Int32) -> Int32
                                     foldladd1 init v = V.foldl add1 init v |])

counterSys :: SysDef ((Signal (V.FSVec D4 Int32)) -> Signal Int32)
counterSys = newSysDef counter "counterFoldlV" ["input"] ["countVal"]

simCounter :: [Int32]
simCounter = simulate counterSys [1+>2+>3+>4+>empty,2+>3+>4+>5+>empty]
