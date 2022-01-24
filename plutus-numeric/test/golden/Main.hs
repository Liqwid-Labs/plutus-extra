module Main (main) where

import PlutusTx.NatRatio (NatRatio)
import PlutusTx.Natural (Natural)
import PlutusTx.Positive (Positive)
import PlutusTx.Ratio (Rational)
import Test.Tasty (defaultMain, testGroup)
import Test.QuickCheck.Plutus.Instances () 
import PlutusTx.Numeric.Orphans ()
import Test.Tasty.Plutus.Golden (goldenData, goldenJSON, goldenToSchema)
import Prelude hiding (Rational)

main :: IO ()
main =
  defaultMain . testGroup "Golden tests" $
    [ goldenJSON @Natural
    , goldenJSON @NatRatio
    , goldenJSON @Positive
    , goldenJSON @Rational
    , goldenData @Natural
    , goldenData @NatRatio
    , goldenData @Positive
    , goldenData @Rational
    , goldenToSchema @Natural
    , goldenToSchema @NatRatio
    , goldenToSchema @Positive
    , goldenToSchema @Rational
    ]
