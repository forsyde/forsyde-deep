{-# LANGUAGE TemplateHaskell #-}

module ZipWithVectorOperator where

import ForSyDe.Deep
import Data.TypeLevel.Num hiding ((+), (*))
import qualified Data.Param.FSVec as V
import Data.Int

zipWithV :: Signal (V.FSVec D4 Int32) -> Signal (V.FSVec D4 Int32) -> Signal (V.FSVec D4 Int32)
zipWithV  = zipWithSY "counterSource" add1
 where add1 = $(newProcFun [d| add1v :: (V.FSVec D4 Int32) -> (V.FSVec D4 Int32) -> (V.FSVec D4 Int32)
                               -- The higher order vector functions can only be
                               -- translated directly when they are the sole
                               -- expression of a function body:
                               add1v v w = V.zipWith (+) v w |])

zipWithVOpSys :: SysDef (Signal (V.FSVec D4 Int32) -> Signal (V.FSVec D4 Int32) -> Signal (V.FSVec D4 Int32))
zipWithVOpSys = newSysDef zipWithV "zipWithVOp" ["a", "b"] ["countVal"]

simZipWithVOpSys :: [(V.FSVec D4 Int32)] -> [(V.FSVec D4 Int32)] -> [(V.FSVec D4 Int32)]
simZipWithVOpSys = simulate zipWithVOpSys
