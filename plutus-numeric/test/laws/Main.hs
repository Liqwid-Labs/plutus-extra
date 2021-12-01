module Main (main) where

import PlutusTx.NatRatio (NatRatio)
import PlutusTx.Natural (Natural)
import PlutusTx.Rational (Rational)
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.Plutus.Laws (
  additiveGroupLaws,
  additiveMonoidLaws,
  additiveSemigroupLaws,
  dataLaws,
  jsonLaws,
  multiplicativeMonoidLaws,
  multiplicativeSemigroupLaws,
  plutusEqLaws,
  plutusEqLawsSubstitution,
  plutusOrdLaws,
  semiringConsistencyLaws,
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
    , additiveSemigroupLaws @Natural
    , additiveSemigroupLaws @Rational
    , additiveSemigroupLaws @NatRatio
    , additiveMonoidLaws @Natural
    , additiveMonoidLaws @Rational
    , additiveMonoidLaws @NatRatio
    , additiveGroupLaws @Rational
    , multiplicativeSemigroupLaws @Natural
    , multiplicativeSemigroupLaws @Rational
    , multiplicativeSemigroupLaws @NatRatio
    , multiplicativeMonoidLaws @Natural
    , multiplicativeMonoidLaws @Rational
    , multiplicativeMonoidLaws @NatRatio
    , semiringConsistencyLaws @Natural
    , semiringConsistencyLaws @Rational
    , semiringConsistencyLaws @NatRatio
    ]
  where
    testMinimum :: QuickCheckTests
    testMinimum = 10000
