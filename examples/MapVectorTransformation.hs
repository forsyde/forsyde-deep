{-# LANGUAGE TemplateHaskell #-}

module MapVectorTransformation where

import ForSyDe.Deep
import Data.TypeLevel.Num hiding ((+))
import qualified Data.Param.FSVec as V
import Data.Int

transfVecCounter :: Signal (V.FSVec D4 Int32)
transfVecCounter  = sourceSY "transfVecCounterSource" add1 (V.copy d4 (0 :: Int32))
 where add1 = $(newProcFun [d| add1v :: (V.FSVec D4 Int32) -> (V.FSVec D4 Int32)
                               -- calls to higher order functions are
                               -- supported, if they have a type signature of
                               -- the form: 
                               --   (map f v::<vectype>)::<rettype>
                               -- This means, the returned value and all
                               -- arguments except the argument function need
                               -- to have proper type signatures
                               add1v v = V.reverse ((V.map add1 (v::(V.FSVec D4 Int32)))::V.FSVec D4 Int32)
                                 where
                                     add1 :: Int32 -> Int32
                                     add1 a = a+1 |])

transfVecCounterSys :: SysDef (Signal (V.FSVec D4 Int32))
transfVecCounterSys = newSysDef transfVecCounter "transfVecCounterMapV" [] ["countVal"]

simTransfVecCounter :: [(V.FSVec D4 Int32)]
simTransfVecCounter = simulate transfVecCounterSys
