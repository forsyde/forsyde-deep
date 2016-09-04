{-# LANGUAGE TemplateHaskell #-}


module MapVectorOperatorSection where

import ForSyDe.Deep
import Data.TypeLevel.Num hiding ((+),(*))
import qualified Data.Param.FSVec as V
import Data.Int

vecOpSecCounter :: Signal (V.FSVec D4 Int32)
vecOpSecCounter  = sourceSY "vecOpSecCounterSource" add1 (V.copy d4 (0 :: Int32))
 where add1 = $(newProcFun [d| add1v :: (V.FSVec D4 Int32) -> (V.FSVec D4 Int32)
                               -- Operator sections (partially applied infix
                               -- operators) are supported if they are enclosed
                               -- in a type signature
                               add1v v = V.map (((+1*4)*5)::Int32 -> Int32) v |])

vecOpSecCounterSys :: SysDef (Signal (V.FSVec D4 Int32))
vecOpSecCounterSys = newSysDef vecOpSecCounter "vecOpSecCounterMapV" [] ["countVal"]

simVecOpSecCounter :: [(V.FSVec D4 Int32)]
simVecOpSecCounter = simulate vecOpSecCounterSys
