ForSyDe's Haskell-embedded Domain Specific Language.
====================================================

DESCRIPTION
-----------

 The ForSyDe (Formal System Design) methodology has been developed
 with the objective to move system design to a higher level of
 abstraction and to bridge the abstraction gap by transformational
 design refinement.
 
 This library provides ForSyDe's implementation as a Haskell-embedded
 Domain Specific Language (DSL). 

 For more information, please see ForSyDe's website:
 https://forsyde.github.io/


INSTALLATION
------------
 
ForSyDe depends on GHC vesions 7.10.3 or 8.0.1 due to the use of
numerous extensions, namely Template Haskell (TH).

It depends on the
[`type-level`](https://github.com/forsyde/type-level) and
[`parameterized-data`](https://github.com/forsyde/parameterized-data)
packages and some others normally bundled with GHC distributions.

For synthesis and simulation of the generated VHDL the Altera toolchain is
supported. Quartus and Modelsim need to be on the PATH for these features to
work.  Additionally the open-source VHDL simulator Ghdl is supported from
version ghdl-0.33 onwards.

This package can be installed with:

* Cabal, provided you have installed the right version of GHC and its
  dependent `cabal-install` package using the commands:

        cabal install # --with-ghc=path/to/ghc-version # installs forsyde-deep
		cabal haddock                                  # generates documentation. Needs Haddock > 2.0
		
* Stack, which takes care of all dependencies and installs everything
  (including the compiler) in a sandboxed environment:
  
        stack install    # installs forsyde-deep
		stack haddock    # generates documentation
		
