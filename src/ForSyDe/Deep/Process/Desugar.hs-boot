{-# LANGUAGE Rank2Types #-}
module ForSyDe.Deep.Process.Desugar (desugarTransform) where

import Language.Haskell.TH
import Data.Generics

desugarTransform :: [Dec] -> Q [Dec]
