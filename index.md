---
layout: default
title: ForSyDe-Deep
description: A Deep-Embedded Synthesizer for ForSyDe Models
isHome: true
---


# Overview

[![Build Status](https://travis-ci.org/forsyde/forsyde-deep.svg?branch=master)](https://travis-ci.org/forsyde/forsyde-deep)
![Development Status](assets/images/active.svg)

The [ForSyDe-Deep]() is the deep-embedded counterpart of [ForSyDe-Shallow]({{site.parent-url}}/forsyde-shallow), and is able to both simulate and synthesize a subset of the ForSyDe language. It currently supports two backend formats for synthesis:

* VHDL code describing digital hardware;
* GraphML structure representing the ForSyDe process network. 

Among the features of ForSyDe-Deep, we can highlight the following:

* it recognizes synchronous (SY) process constructors and is able to generate corresponding backend templates.
* it supports skeletons on [vectors](http://hackage.haskell.org/package/parameterized-data/docs/Data-Param-FSVec.html) which are higher-order functions which create regular parallel structures of components.
* it parses a subset of the Haskell language, captured by functions passed as arguments to process constructors, using [Template Haskell](https://wiki.haskell.org/Template_Haskell), and is able to synthesize equivalent backend code.
* it provides a rich set of utilities for compiling or simulating the generated VHDL code using [ModelSim](https://www.mentor.com/products/fv/modelsim/) or the [Intel Quartus](https://www.intel.com/content/www/us/en/programmable/downloads/download-center.html) tool suite.

# Quick-Start

Due to numerous dependencies on [Template Haskell](https://wiki.haskell.org/Template_Haskell), ForSyDe-Deep is only compatible with versions 7.10.3 or 8.0.1 of the GHC compiler. Because of this, we recommend installing the libraries and tools as a [Stack](https://docs.haskellstack.org/en/stable/README/) project. The quickest way to test ForSyDe-Deep is to  [clone](https://github.com/forsyde/forsyde-deep) or [download](https://api.github.com/repos/forsyde/forsyde-deep/zipball/master) the project, as it has all the dependencies already set, and run one of the available [examples](https://github.com/forsyde/forsyde-deep/tree/master/examples) in a sandboxed environment. To do this, after you acquire the source code, type in

	cd path/to/forsyde-deep
	stack update
	stack install
	
and wait until the installation is complete. Load one of the example designs in an interpreter session. E.g.:

    stack ghci examples/FoldlVector.hs

which contains the design of a combinational system which sums  the elements of a vector with length 4, defined along the lines of:

{% highlight haskell %}
foldingAdder :: Signal (FSVec D4 Int32) -> Signal Int32
foldingAdder  = mapSY "counterSource" add1
  where add1 = $(newProcFun [d| add1v :: (FSVec D4 Int32) -> Int32
                                add1v v = foldladd1 0 v
                                  where 
                                    foldladd1 :: Int32 -> (FSVec D4 Int32) -> Int32
                                    foldladd1 init v = foldl (+) init v |])

foldingAdderSys :: SysDef ((Signal (FSVec D4 Int32)) -> Signal Int32)
foldingAdderSys = newSysDef foldingAdder "foldingAdder" ["input"] ["countVal"]
{% endhighlight %}
	
Inside the interpreter session load the `ForSyDe.Deep` and the `Data.Param.FSVec` libraries

	> :m +ForSyDe.Deep Data.Param.FSVec

Let's create three test `FSVec`tors to simulate against the `foldingAdder` process:

	> let x1 = 1 +> 2 +> 3 +> 4 +> empty     -- <1,2,3,4>
	> let x2 = 2 +> 3 +> 4 +> 5 +> empty     -- <2,3,4,5>
	> let x3 = 10 +> 11 +> 12 +> 13 +> empty -- <10,11,12,13>
	> simulate foldingAdderSys  $ [x1, x2, x3]
	[10,14,46]

You can also dump the complete functional VHDL files or the GraphML structure directly with:

	> writeVHDL foldingAdderSys
	> writeGraphML foldingAdderSys

Check out the dumped files. You can open them in any appropriate editor or tool suite. To make use of the utilities for passing the design to tools like Quartus or ModelSim, or for tips on visualizing the system structure please follow the tutorials pointed below.

# Documentation and Resources

Here you can find links to further documentation resources:

 * [**The setup page**](setup) contains detailed instructions on how to install and use the libraries.

 * [**A getting started tutorial**](getting_started).

 * [**A more detailed tutorial**](forsyde-deep-tutorial) on advanced usage of ForSyDe-Deep.

 * [**The API documentation**](http://hackage.haskell.org/package/forsyde-deep) generated with Haddock.

