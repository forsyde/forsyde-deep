---
layout: default
title: Installation and usage
permalink: setup.html
---

# Dependencies

Depending on how you are planning to use the ForSyDe-Deep libraries, you might need to fulfill some prerequisites. Before installing, please check for the following dependencies:

### General dependencies

These are required to acquire, install and use the base ForSyDe-Deep libraries:

 * The [Haskell Platform](https://www.haskell.org/platform/). The libraries usually support the latest `ghc` versions, but you can check the tested versions in the   [`forsyde-deep.cabal`](https://github.com/forsyde/forsyde-deep/blob/master/forsyde-deep.cabal) file in case the installation does not succeed.
 
 * [Git](https://git-scm.com/downloads) if you want to clone the whole repository, and not just download the sources. 
 
Library dependencies are taken care of by the [Cabal](https://www.haskell.org/cabal/) or [Stack](https://docs.haskellstack.org/en/stable/README/) package managers shipped with [Haskell Platform](https://www.haskell.org/platform/).

### Using the Intel (Altera) Quartus Tool Suite

The ForSyDe-Deep library provides a number of utility functions which plug ForSyDe designs into Intel (formerly Altera) FPGA development flows, by calling tools such as Quartus or ModelSim. Needless to say, if you plan to use these utilities, you need to install them and make sure the binaries can be accessed from your system `PATH`. Otherwise, the installation will complain that it cannot see these tools and will provide further instructions.

ForSyDe-Deep has been tested with [Quartus II version 13.0sp1](https://www.intel.com/content/www/us/en/programmable/downloads/download-center.html) and [Quartus Prime version 18.0](https://www.intel.com/content/www/us/en/programmable/downloads/download-center.html).

Additionally the open-source VHDL simulator [Ghdl](http://ghdl.free.fr/) is supported from version ghdl-0.33 onward.


### Visualizing GraphML structures

[GraphML](graphml.graphdrawing.org) is one of the formats dumped by ForSyDe-Deep. To visualize the process network structures, any GraphML visualizing tool would do, however we recommend [yEd](http://www.yworks.com/en/products_yed_about.html) provided by [yWorks](http://www.yworks.com/). We include some [tips on how to use this tool](forsyde-deep-tutorial#obtaining-diagrams-of-forsyde) in one of our tutorials.

# Installation

ForSyDe depends on GHC versions 7.10.3 or 8.0.1 due to the use of numerous extensions, namely Template Haskell (TH). As such, all tools associated with the GHC compiler suite need to be compatible with these versions, especially the `cabal-install` tool:

* `cabal-install-1.22` works with `ghc-7.10.3`
* `cabal-install-1.24` works with `ghc-8.0.1`

It also depends on the
[`type-level`](https://github.com/forsyde/type-level) and
[`parameterized-data`](https://github.com/forsyde/parameterized-data)
packages and some others normally bundled with GHC distributions.

For synthesis and simulation of the generated VHDL the Altera toolchain is
supported. 
This package can be installed with:

* Cabal, provided you have installed **the right version** of GHC and its
  dependent `cabal-install` package using the commands:

        cabal install  # --with-ghc=path/to/ghc-version # installs forsyde-deep
		cabal haddock  # generates documentation. Needs Haddock > 2.0
		
* Stack, which takes care of all dependencies and installs everything
  (including the compiler) in a sandboxed environment:
  
        stack install    # installs forsyde-deep
		stack haddock    # generates documentation
		
* either Cabal or Stack directly from [HackageDB](http://hackage.haskell.org/package/forsyde-deep). However, for this to work, you need to either:
  1. have the right prerequisites installed (see above)
  2. write a proper `stack.yml` configuration to help Stack create the proper sandbox environment, along the lines of [this one](https://github.com/forsyde/forsyde-deep/blob/master/stack.yaml).
