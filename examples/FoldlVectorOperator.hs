{-# LANGUAGE TemplateHaskell #-}

-- A counter, the simplest system with which to test netlist loops

module FoldlVectorOperator where

import ForSyDe.Deep
import Data.TypeLevel.Num hiding ((+))
import qualified Data.Param.FSVec as V
import Data.Param.FSVec ((+>), empty)
import Data.Int

foldingAdder :: Signal (V.FSVec D4 Int32) -> Signal Int32
foldingAdder  = mapSY "counterSource" add1
 where add1 = $(newProcFun [d| add1v :: (V.FSVec D4 Int32) -> Int32
                               add1v v = foldladd1 0 v
                                 where
                                     -- The higher order vector functions can only be
                                     -- translated directly when they are the sole
                                     -- expression of a function body:
                                     foldladd1 :: Int32 -> (V.FSVec D4 Int32) -> Int32
                                     foldladd1 init v = V.foldl (+) init v |])

foldingAdderOpSys :: SysDef ((Signal (V.FSVec D4 Int32)) -> Signal Int32)
foldingAdderOpSys = newSysDef foldingAdder "foldingAdderOperator" ["input"] ["countVal"]

simFoldingAdderOp :: [(V.FSVec D4 Int32)] -> [Int32]
simFoldingAdderOp = simulate foldingAdderOpSys
