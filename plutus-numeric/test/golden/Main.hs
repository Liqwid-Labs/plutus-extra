module Main (main) where

import PlutusTx.NatRatio (NatRatio)
import PlutusTx.Natural (Natural)
import PlutusTx.Rational (Rational)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Plutus.Golden (goldenData, goldenJSON, goldenToSchema)
import Prelude hiding (Rational)

main :: IO ()
main =
  defaultMain . testGroup "Golden tests" $
    [ goldenJSON @Natural
    , goldenJSON @NatRatio
    , goldenJSON @Rational
    , goldenData @Natural
    , goldenData @NatRatio
    , goldenData @Rational
    , goldenToSchema @Natural
    , goldenToSchema @NatRatio
    , goldenToSchema @Rational
    ]
