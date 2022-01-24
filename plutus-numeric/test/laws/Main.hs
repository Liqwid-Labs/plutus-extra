module Main (main) where

import PlutusTx.NatRatio (NatRatio)
import PlutusTx.Natural (Natural)
import PlutusTx.Numeric.Laws (
  additiveHemigroupLaws,
  euclideanClosedLaws,
  euclideanClosedSignedLaws,
  integralDomainLaws,
  multiplicativeGroupLaws,
  scaleNatLaws,
 )
import PlutusTx.Positive (Positive)
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.Plutus.Laws (
  additiveMonoidLaws,
  additiveSemigroupLaws,
  dataLaws,
  jsonLaws,
  laws,
  multiplicativeMonoidLaws,
  multiplicativeSemigroupLaws,
  plutusEqLaws,
  plutusEqLawsSubstitution,
  plutusOrdLaws,
  semiringConsistencyLaws,
 )
import Test.Tasty.QuickCheck (QuickCheckTests)
import PlutusTx.Ratio (Rational)
import Test.QuickCheck.Plutus.Instances ()
import Prelude hiding (Rational)

main :: IO ()
main =
  defaultMain . adjustOption (max testMinimum) . testGroup "Laws" $
    [ jsonLaws @Natural
    , jsonLaws @NatRatio
    , jsonLaws @Positive
    , dataLaws @Natural
    , dataLaws @NatRatio
    , dataLaws @Positive
    , plutusEqLaws @Natural
    , plutusEqLawsSubstitution @Natural
    , plutusEqLaws @Positive
    , plutusEqLawsSubstitution @Positive
    , plutusEqLaws @NatRatio
    , plutusEqLawsSubstitution @NatRatio
    , plutusOrdLaws @Natural
    , plutusOrdLaws @Positive
    , plutusOrdLaws @NatRatio
    , additiveSemigroupLaws @Natural
    , additiveSemigroupLaws @Positive
    , additiveSemigroupLaws @NatRatio
    , additiveMonoidLaws @Natural
    , additiveMonoidLaws @NatRatio
    , multiplicativeSemigroupLaws @Natural
    , multiplicativeSemigroupLaws @Positive
    , multiplicativeSemigroupLaws @NatRatio
    , multiplicativeMonoidLaws @Natural
    , multiplicativeMonoidLaws @Positive
    , multiplicativeMonoidLaws @Rational
    , multiplicativeMonoidLaws @NatRatio
    , semiringConsistencyLaws @Natural
    , semiringConsistencyLaws @NatRatio
    , laws @Natural "Additive hemigroup" additiveHemigroupLaws
    , laws @NatRatio "Additive hemigroup" additiveHemigroupLaws
    , laws @Natural "EuclideanClosed" euclideanClosedLaws
    , laws @Integer "EuclideanClosed" euclideanClosedSignedLaws
    , laws @Rational "Multiplicative group" multiplicativeGroupLaws
    , laws @NatRatio "Multiplicative group" multiplicativeGroupLaws
    , laws @Integer "IntegralDomain" integralDomainLaws
    , laws @Rational "IntegralDomain" integralDomainLaws
    , laws @Integer "scaleNat" scaleNatLaws
    , laws @Natural "scaleNat" scaleNatLaws
    , laws @NatRatio "scaleNat" scaleNatLaws
    , laws @Rational "scaleNat" scaleNatLaws
    ]
  where
    testMinimum :: QuickCheckTests
    testMinimum = 10000
