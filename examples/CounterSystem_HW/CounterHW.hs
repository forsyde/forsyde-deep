{-# LANGUAGE TemplateHaskell #-}

module CounterHW (Direction, counterSys) where

import ForSyDe.Deep
import Data.Int

type Direction = Bit

nextStateFun :: ProcFun (Int8 -> Direction -> Int8)
nextStateFun = $(newProcFun
   [d| nextState state dir = if dir == H then
                                 if state < 9 then
                                     state + 1
                                 else
                                     0
                             else
                                 if state == 0 then
                                     9
                                 else
                                     state - 1
     |])

counterProc :: Signal Direction -> Signal Int8 
counterProc = scanldSY "counterProc" nextStateFun 0


counterSys :: SysDef (Signal Direction -> Signal Int8)
counterSys = newSysDef counterProc "Counter" ["direction"] ["number"]	
