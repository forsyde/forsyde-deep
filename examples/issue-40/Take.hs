{-# LANGUAGE TemplateHaskell #-}
module Take where
import ForSyDe.Deep
import Data.Int
import Data.TypeLevel
import Data.Param.FSVec

takeProcFun :: ProcFun(FSVec D3 Int32 -> FSVec D1 Int32)
takeProcFun = $(newProcFun
    [d| takeProcFun :: FSVec D3 Int32 -> FSVec D1 Int32
        takeProcFun x = Data.Param.FSVec.take d1 x
    |])

takeProc :: Signal (FSVec D3 Int32) -> Signal (FSVec D1 Int32)
takeProc = mapSY "proc1" takeProcFun

takeSysDef :: SysDef(Signal (FSVec D3 Int32) -> Signal (FSVec D1 Int32))
takeSysDef = newSysDef takeProc "mingzi" ["in"] ["out"]

x1 = (1 :: Int32) +> (2 :: Int32) +> (3 :: Int32) +> empty
x2 = (4 :: Int32) +> (5 :: Int32) +> (6 :: Int32) +> empty

testSimulateForSyDe = simulate takeSysDef [x1, x2] 

-- IMPORTANT: Programming the DE10 Standard:
-- > quartus_pgm -c DE-SoC -m JTAG -o "p;./And2Sys/vhdl/And2Sys.sof@2"
generateHW_DE_10_Standard :: IO ()
generateHW_DE_10_Standard = writeVHDLOps vhdlOps takeSysDef
 where vhdlOps = defaultVHDLOps{execQuartus=Just quartusOps}
       quartusOps = QuartusOps{action=FullCompilation,
                               fMax=Just 24, -- in MHz
                               fpgaFamiliyDevice=Just ("Cyclone V",
                                                       Just "5CSXFC6D6F31C6"),
                               -- Possibility for Pin Assignments
                               pinAssigs=[
                                          -- ("a", "PIN_AB30"),  -- SW0
                                          -- ("b", "PIN_Y27"),  -- SW1
                                          -- ("clock","PIN_AJ4"),    -- KEY[0]
                                          -- ("out[6]","PIN_W17"),   -- HEX0[0]
                                          -- ("out[5]","PIN_V18"),   -- HEX0[1]
                                          -- ("out[4]","PIN_AG17"),  -- HEX0[2]
                                          -- ("out[3]","PIN_AG16"),  -- HEX0[3]
                                          -- ("out[2]","PIN_AH17"),  -- HEX0[4]
                                          -- ("out[1]","PIN_AG18"),  -- HEX0[5]
                                          -- ("out[0]","PIN_AH18"),  -- HEX0[6]
                                          -- ("y", "PIN_AA24")   -- LEDR[0]
                                         ]
                              }
