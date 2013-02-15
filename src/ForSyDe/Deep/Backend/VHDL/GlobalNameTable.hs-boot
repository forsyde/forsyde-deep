module ForSyDe.Deep.Backend.VHDL.GlobalNameTable (globalNameTable) where


import Language.Haskell.TH
import qualified ForSyDe.Deep.Backend.VHDL.AST as VHDL

globalNameTable :: [(Name, (Int, [VHDL.Expr] -> VHDL.Expr ) )]
