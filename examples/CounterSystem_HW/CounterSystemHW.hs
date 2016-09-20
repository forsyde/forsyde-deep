{-# LANGUAGE TemplateHaskell #-}

module CounterSystemHW where

import ForSyDe.Deep
import CounterHW
import SevenSegmentDecoderHW
import ForSyDe.Deep.Bit
import Data.Param.FSVec
import Data.Int
import Data.TypeLevel.Num.Reps
import Data.TypeLevel.Num.Aliases


systemProc :: Signal Direction -> Signal (FSVec D7 Bit)
systemProc dir = sevenSeg
   where
      sevenSeg   = (instantiate "sevenSegDec" sevenSegDecSys) counterOut
      counterOut = (instantiate "counter" counterSys) dir

system :: SysDef (Signal Direction -> Signal (FSVec D7 Bit))
system = newSysDef systemProc "system" ["in"] ["out"]


-- Hardware Generation
compileQuartus_CounterSystem :: IO ()
compileQuartus_CounterSystem = writeVHDLOps vhdlOps system
 where vhdlOps = defaultVHDLOps{execQuartus=Just quartusOps}
       quartusOps = QuartusOps{action=FullCompilation,
                               fMax=Just 50, -- in MHz
                               fpgaFamiliyDevice=Just ("CycloneII",
                                                       Just "EP2C35F672C6"),
                               -- Possibility for Pin Assignments
                               pinAssigs=[("in", "PIN_N25"),  -- SW0
                                          ("resetn", "PIN_N26"),  -- SW1
                                          ("clock","PIN_G26"), -- KEY[0]
                                          ("out[6]","PIN_AF10"), -- HEX0[0]
                                          ("out[5]","PIN_AB12"), -- HEX0[1]
                                          ("out[4]","PIN_AC12"), -- HEX0[2]
                                          ("out[3]","PIN_AD11"), -- HEX0[3]
                                          ("out[2]","PIN_AE11"), -- HEX0[4]
                                          ("out[1]","PIN_V14"),  -- HEX0[5]
                                          ("out[0]","PIN_V13")   -- HEX0[6]
                                         ]
                              }
