module Main (main) where

import PlutusTx.Natural (Natural)
import PlutusTx.NatRatio (NatRatio)
import Test.Tasty (defaultMain, testGroup, adjustOption)
import Test.Tasty.Plutus.Laws (jsonLaws)
import Test.Tasty.QuickCheck (QuickCheckTests)

main :: IO ()
main = defaultMain . adjustOption (max testMinimum) . testGroup "Laws" $ [
  jsonLaws @Natural,
  jsonLaws @NatRatio
  ]
  where
    testMinimum :: QuickCheckTests
    testMinimum = 10000
