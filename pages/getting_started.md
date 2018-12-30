---
layout: default
title: Getting Started with ForSyDe-Deep
permalink: getting_started.html
---

# Getting Started with ForSyDe-Deep

> Ingo Sander  
> Royal Institute of Technology  
> Stockholm, Sweden  
> ingo@kth.se

## Introduction

ForSyDe (Formal System Design) has been developed as system design methodology for heterogeneous embedded systems. The initial idea of ForSyDe has been to start with an abstract and formal specification model, that is then refined using well-defined design transformations into a low-level implementation model, and finally mapped into an implementation in hardware or software [[Sander and Jantsch, 2004]]({{ site.parent-url }}/publications.html#2004). Initially ForSyDe only used the synchronous model of computation (MoC), but has later been extended to cover additional models of computation, which can be integrated into one executable heterogeneous model of computation. For the synchronous model of computation there exists a synthesis back-end, which translates an executable synchronous ForSyDe model into the corresponding VHDL-code that can then further be synthesized using a commercial logic synthesis tool.

This tutorial is a direct follow-up of the tutorial [Getting Started with ForSyDe-Shallow]({{ site.parent-url }}/forsyde-shallow/getting_started) and continues from where it left. If you have not gone through the aforementioned tutorial, please do so now and return to this page once you have finished it. 

# Hardware Design in ForSyDe

The [previous tutorial]({{ site.parent-url }}/forsyde-shallow/getting_started) illustrated how to model systems in ForSyDe. Naturally, we would like to generate hardware or software from these models, but the so called shallow-embedded implementation of ForSyDe discussed in the previous section does not give access to the internal structure of the ForSyDe model. Thus the only way to develop a synthesis tool would be to write a traditional compiler, which requires an enormous engineering effort.

Instead we have chosen to develop a second implementation, called deep-embedded ForSyDe, which gives access to the internal structure of the ForSyDe model. Based on this structural information ForSyDe’s embedded compiler can perform different analysis and transformations, like the simulation of the system model or the synthesis of the model to a target language like VHDL.

In order to have access to the internal data structure, the designer has to write the code in a slightly different way, which requires more effort. However, the deep-embedded implementation follows strictly the ideas of ForSyDe as presented in the previous section. Also deep-embedded synthesizable models can be co-simulated with shallow-embedded models.

### Seven Segment Decoder

In order to introduce the deep-embedded version of ForSyDe we start with the design of a seven segment decoder. To see the difference between the shallow-embedded and the deep-embedded version of ForSyDe, we will first present the shallow-embedded version of the seven segment decoder in the following listing:

{% highlight haskell %}
module SevenSegmentDecoder (sevenSegDec) where
import ForSyDe.Shallow
import ForSyDe.Bit
import Data.Param.FSVec
import Data.Int
import Data.TypeLevel.Num.Reps
import Data.TypeLevel.Num.Aliases

-- L turns LED on
decode :: Int8 -> FSVec D7 Bit
decode 0 = H +> L +> L +> L +> L +> L +> L +> empty
decode 1 = H +> H +> H +> H +> L +> L +> H +> empty
decode 2 = L +> H +> L +> L +> H +> L +> L +> empty
decode 3 = L +> H +> H +> L +> L +> L +> L +> empty
decode 4 = L +> L +> H +> H +> L +> L +> H +> empty
decode 5 = L +> L +> H +> L +> L +> H +> L +> empty
decode 6 = L +> L +> L +> L +> L +> H +> L +> empty
decode 7 = H +> H +> H +> H +> L +> L +> L +> empty
decode 8 = L +> L +> L +> L +> L +> L +> L +> empty
decode 9 = L +> L +> H +> L +> L +> L +> L +> empty
decode _ = H +> H +> H +> H +> H +> H +> H +> empty

sevenSegDec :: Signal Int8 -> Signal (FSVec D7 Bit)
sevenSegDec = mapSY decode
{% endhighlight %}

We want to point out that this shallow-embedded model is already prepared for hardware synthesis. It uses a data type `Bit` with the values `L` and `H`, which can be synthesized in the deep-embedded version of ForSyDe. Even more important is the use of the fixed-sized vector data type `FSVec` instead of a list or vector data type. For hardware synthesis it is of essential importance to give the exact size of a vector, which is possible with `FSVec`. Here `D7` gives the size of the fixed-sized vector, which in this case is a vector of seven bits.

The deep-embedded ForSyDe implementation uses ​[Template Haskell](https://wiki.haskell.org/Template_Haskell), which allows to access the internal structure of the model. In order to enable the Template Haskell extension, we prepend the following pragma to our model.

{% highlight haskell %}
{-# LANGUAGE TemplateHaskell #-}
module SevenSegmentDecoderHW (sevenSegDecSys) where

import ForSyDe.Deep
import ForSyDe.Bit
import Data.Param.FSVec
import Data.Int
import Data.TypeLevel.Num.Reps
import Data.TypeLevel.Num.Aliases
{% endhighlight %}

Observe that the deep-embedded implementation of ForSyDe is imported using the command `import ForSyDe.Deep`, while the shallow-embedded implementation is imported using `import ForSyDe.Shallow`. The other `import` commands are used to import the data types `Bit`, `Int8`, and the fixed-sized vector data type.

Also in deep-embedded ForSyDe processes are created by means of process constructors taking functions and variables as arguments. However, we define an additional level, which is called *system definition* and creates a reusable synthesizable component.

In the deep-embedded world the functions that are arguments of the process constructor are called *process functions* and have a different data type `ProcFun`. Since we need access to the internal representation of the function Template Haskell is used for the definition of the function.

{% highlight haskell %}
decodeFun :: ProcFun (Int8 -> FSVec D7 Bit)
decodeFun
  = $(newProcFun
       [d| decode :: Int8 -> FSVec D7 Bit
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
{% endhighlight %}

The main difference to the shallow-embedded version is the special syntax. First the function has the data type `ProcFun (Int8 -> FSVec D7 Bit)` instead of `Int8 -> FSVec D7 Bit`. Then we declare the computation function inside a code pattern that allows to have access to the internal structure of the model, the abstract syntax tree (AST). The code pattern is `$(newProcFun [d| ... |])` Inside this pattern the function is declared in the same way as before. In contrast to the shallow-embedded version, the type for the function, here `decode :: Int8 -> FSVec D7 Bit`, needs to be provided.

The next level declares the process using process constructor and process function. In contrast to the shallow-embedded version an additional label needs to be provided to distuinguish different processes.

{% highlight haskell %}
sevenSegDecProc :: Signal Int8 -> Signal (FSVec D7 Bit)
sevenSegDecProc = mapSY "decode" decodeFun
{% endhighlight %}

Finally the system definition is created. The system is defined providing the process, a label, a list of input port names, and a list of output port names.

{% highlight haskell %}
sevenSegDecSys :: SysDef (Signal Int8 -> Signal (FSVec D7 Bit))
sevenSegDecSys = newSysDef sevenSegDecProc "sevenSegDec" ["in"] ["out"]
{% endhighlight %}

The deep-embedded ForSyDe compiler makes use of the knowledge of the internal structure of a system by providing different backends that operate on the internal structure of the system. The first backend is the simulation backend. The seven segment decoder can be simulated using the following command:

	*SevenSegmentDecoderHW> simulate sevenSegDecSys [4,0,2]
	[<L,L,H,H,L,L,H>,<H,L,L,L,L,L,L>,<L,H,L,L,H,L,L>]
	
Observe that the deep-embedded compiler uses plain Haskell lists for signals.

More interesting is the second backend that generates VHDL-code for a system.

	*SevenSegmentDecoderHW> writeVHDL sevenSegDecSys

This generates the VHDL-code for the seven segment decoder. The VHDL-code is located in the directory `sevenSegDec/vhdl`. Further we can invoke the Altera Quartus tool with design options using the following command:

{% highlight haskell %}
compileQuartus_sevenSegDecSys :: IO ()
compileQuartus_sevenSegDecSys = writeVHDLOps vhdlOps sevenSegDecSys
  where vhdlOps = defaultVHDLOps{execQuartus=Just quartusOps}
        quartusOps
          = QuartusOps{action=FullCompilation,
                       -- fMax is not needed for combinational circuits
                       fMax=Just 50, -- in MHz
                       fpgaFamiliyDevice=Just ("CycloneII",
                                                Just "EP2C35F672C6"),
                       -- Possibility for Pin Assignments
                       pinAssigs=[("in[0]", "PIN_N25"),		-- SW0
                                  ("in[1]", "PIN_N26"),		-- SW1
                                  ("in[2]", "PIN_P25"),		-- SW2
                                  ("in[3]", "PIN_AE14"),	-- SW3
                                  ("in[4]", "PIN_AF14"),	-- SW4
                                  ("in[5]", "PIN_AD13"),	-- SW5
                                  ("in[6]", "PIN_AC13"),	-- SW6
                                  ("in[7]", "PIN_C13"),		-- SW7
                                  ("out[6]","PIN_AF10"),	-- HEX0[0]
                                  ("out[5]","PIN_AB12"),	-- HEX0[1]
                                  ("out[4]","PIN_AC12"),	-- HEX0[2]
                                  ("out[3]","PIN_AD11"),	-- HEX0[3]
                                  ("out[2]","PIN_AE11"),	-- HEX0[4]
                                  ("out[1]","PIN_V14"),		-- HEX0[5]
                                  ("out[0]","PIN_V13")		-- HEX0[6]
                                 ]
                      }
{% endhighlight %}

In case Quartus is installed an implementation of the seven segment decoder `sevenSegDec.sof` is generated, which is located in the directory `sevenSegDec/vhdl`. The pin assignments are compatible with Altera’s DE2 board.

### Counter

As an example for a sequential circuit we will implement a simple counter, which counts up- or downwards between 0 and 9.

{% highlight haskell %}
{-# LANGUAGE TemplateHaskell #-}
module CounterHW (Direction, counterSys) where

import ForSyDe
import Data.Int

type Direction = Bit

nextStateFun :: ProcFun (Int8 -> Direction -> Int8)
nextStateFun = $(newProcFun
  [d| nextState state dir = if dir == H
                            then if state < 9
                                 then state + 1
                                 else 0
                            else if state == 0
                                 then 9
                                 else state - 1
    |])

counterProc :: Signal Direction -> Signal Int8
counterProc = scanldSY "counterProc" nextStateFun 0

counterSys :: SysDef (Signal Direction -> Signal Int8)
counterSys = newSysDef counterProc "Counter" ["direction"] ["number"]
{% endhighlight %}

### Structured Hardware Design

Finally we will develop use instances of the seven segment decoder and the counter to create a new synthesizable design that counts up- and downwards and shows the actual state on the seven-segment display.

{% highlight haskell %}
systemProc :: Signal Direction -> Signal (FSVec D7 Bit)
systemProc dir = sevenSeg
  where
    sevenSeg   = (instantiate "sevenSegDec" sevenSegDecSys) counterOut
    counterOut = (instantiate "counter" counterSys) dir

system :: SysDef (Signal Direction -> Signal (FSVec D7 Bit))
system = newSysDef systemProc "system" ["in"] ["out"]
{% endhighlight %}

Here we see that a composite process `systemProc` is created by describing a ’netlist’ as a set of equations. In this set of equations the systems `sevenSegDecSys` and `counterSys` are instantiated. Finally the new system `system` is created by a system definition using the process `systemProc`.

The full synthesizable code for the system is given below including parameters for the Quartus tool targeting the DE2 board.

{% highlight haskell %}
{-# LANGUAGE TemplateHaskell #-}

import ForSyDe.Deep
import CounterHW
import SevenSegmentDecoderHW
import ForSyDe.Bit
import Data.Param.FSVec
import Data.Int
import Data.TypeLevel.Num.Reps
import Data.TypeLevel.Num.Aliases

systemProc :: Signal Direction -> Signal (FSVec D7 Bit)
systemProc dir = sevenSeg
  where
    sevenSeg   = (instantiate "sevenSegDec" sevenSegDecSys)
    counterOut = (instantiate "counter" counterSys) dir

system :: SysDef (Signal Direction -> Signal (FSVec D7 Bit))
system = newSysDef systemProc "system" ["in"] ["out"]

-- Hardware Generation
compileQuartus_CounterSystem :: IO ()
compileQuartus_CounterSystem = writeVHDLOps vhdlOps system
  where
    vhdlOps = defaultVHDLOps{execQuartus=Just quartusOps}
    quartusOps
      = QuartusOps{action=FullCompilation,
                   fMax=Just 50, -- in MHz 
                   fpgaFamiliyDevice=Just ("CycloneII",
                                           Just "EP2C35F672C6"),
                   -- Possibility for Pin Assignments
                   pinAssigs=[("in", "PIN_N25"),     -- SW0
                              ("resetn", "PIN_N26"), -- SW1
                              ("clock","PIN_G26"),   -- KEY[0]
                              ("out[6]","PIN_AF10"), -- HEX0[0]
                              ("out[5]","PIN_AB12"), -- HEX0[1]
                              ("out[4]","PIN_AC12"), -- HEX0[2]
                              ("out[3]","PIN_AD11"), -- HEX0[3]
                              ("out[2]","PIN_AE11"), -- HEX0[4]
                              ("out[1]","PIN_V14"),  -- HEX0[5]
                              ("out[0]","PIN_V13")   -- HEX0[6]
                             ]
                  }
{% endhighlight %}
