module Main (main) where

import PlutusTx.NatRatio (NatRatio)
import PlutusTx.Natural (Natural)
import PlutusTx.Rational (Rational)
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.Plutus.Laws (
  dataLaws,
  jsonLaws,
  plutusEqLaws,
  plutusEqLawsSubstitution,
  plutusOrdLaws,
 )
import Test.Tasty.QuickCheck (QuickCheckTests)
import Prelude hiding (Rational)

main :: IO ()
main =
  defaultMain . adjustOption (max testMinimum) . testGroup "Laws" $
    [ jsonLaws @Natural
    , jsonLaws @NatRatio
    , dataLaws @Natural
    , dataLaws @NatRatio
    , plutusEqLaws @Natural
    , plutusEqLawsSubstitution @Natural
    , plutusEqLaws @Rational
    , plutusEqLawsSubstitution @Rational
    , plutusEqLaws @NatRatio
    , plutusEqLawsSubstitution @NatRatio
    , plutusOrdLaws @Natural
    , plutusOrdLaws @Rational
    , plutusOrdLaws @NatRatio
    ]
  where
    testMinimum :: QuickCheckTests
    testMinimum = 10000
