module Main (main) where

import PlutusTx.NatRatio (NatRatio)
import PlutusTx.Natural (Natural)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Plutus.Golden (goldenData, goldenJSON, goldenToSchema)

main :: IO ()
main =
  defaultMain . testGroup "Golden tests" $
    [ goldenJSON @Natural
    , goldenJSON @NatRatio
    , goldenData @Natural
    , goldenData @NatRatio
    , goldenToSchema @Natural
    , goldenToSchema @NatRatio
    ]
