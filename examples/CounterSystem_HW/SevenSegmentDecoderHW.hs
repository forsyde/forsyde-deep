{-# LANGUAGE TemplateHaskell #-}

module SevenSegmentDecoderHW (sevenSegDecSys) where

import ForSyDe.Deep
import ForSyDe.Deep.Bit
import Data.Param.FSVec
import Data.Int
import Data.TypeLevel.Num.Reps
import Data.TypeLevel.Num.Aliases

-- L turns LED on

decodeFun :: ProcFun (Int8 -> FSVec D7 Bit)
decodeFun 
   = $(newProcFun [d|decode :: Int8 -> FSVec D7 Bit
                     decode x 
                       = case x of 
                           0 -> H +> L +> L +> L +> L +> L +> L +> empty
                           1 -> H +> H +> H +> H +> L +> L +> H +> empty
                           2 -> L +> H +> L +> L +> H +> L +> L +> empty
                           3 -> L +> H +> H +> L +> L +> L +> L +> empty
                           4 -> L +> L +> H +> H +> L +> L +> H +> empty
                           5 -> L +> L +> H +> L +> L +> H +> L +> empty
                           6 -> L +> L +> L +> L +> L +> H +> L +> empty
                           7 -> H +> H +> H +> H +> L +> L +> L +> empty
                           8 -> L +> L +> L +> L +> L +> L +> L +> empty
                           9 -> L +> L +> H +> L +> L +> L +> L +> empty
                           _ -> H +> H +> H +> H +> H +> H +> H +> empty
                              |])

sevenSegDecProc :: Signal Int8 -> Signal (FSVec D7 Bit)
sevenSegDecProc = mapSY "decode" decodeFun

sevenSegDecSys :: SysDef (Signal Int8 -> Signal (FSVec D7 Bit))
sevenSegDecSys = newSysDef sevenSegDecProc "sevenSegDec" ["in"] ["out"] 
 
-- Hardware Generation
compileQuartus_sevenSegDecSys :: IO ()
compileQuartus_sevenSegDecSys = writeVHDLOps vhdlOps sevenSegDecSys
 where vhdlOps = defaultVHDLOps{execQuartus=Just quartusOps}
       quartusOps = QuartusOps{action=FullCompilation,
                               fMax=Just 50, -- in MHz
                               fpgaFamiliyDevice=Just ("CycloneII",
                                                       Just "EP2C35F672C6"),
                               -- Possibility for Pin Assignments
                               pinAssigs=[("in[0]", "PIN_N25"),  -- SW0
                                          ("in[1]", "PIN_N26"),  -- SW1
                                          ("in[2]", "PIN_P25"),  -- SW2
                                          ("in[3]", "PIN_AE14"), -- SW3
                                          ("in[4]", "PIN_AF14"), -- SW4
                                          ("in[5]", "PIN_AD13"), -- SW5
                                          ("in[6]", "PIN_AC13"), -- SW6
                                          ("in[7]", "PIN_C13"),  -- SW7
                                          ("out[6]","PIN_AF10"), -- HEX0[0]
                                          ("out[5]","PIN_AB12"), -- HEX0[1]
                                          ("out[4]","PIN_AC12"), -- HEX0[2]
                                          ("out[3]","PIN_AD11"), -- HEX0[3]
                                          ("out[2]","PIN_AE11"), -- HEX0[4]
                                          ("out[1]","PIN_V14"),  -- HEX0[5]
                                          ("out[0]","PIN_V13")   -- HEX0[6]
                                         ]
                              }
