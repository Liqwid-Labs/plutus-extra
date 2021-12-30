module Main (main) where

import PlutusTx.NatRatio (NatRatio)
import PlutusTx.Natural (Natural)
import PlutusTx.Numeric.Laws (
  additiveHemigroupLaws,
  euclideanClosedLaws,
  euclideanClosedSignedLaws,
  integralDomainLaws,
  multiplicativeGroupLaws,
 )
import PlutusTx.Positive (Positive)
import PlutusTx.Rational (Rational)
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.Plutus.Laws (
  additiveGroupLaws,
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
import Prelude hiding (Rational)

main :: IO ()
main =
  defaultMain . adjustOption (max testMinimum) . testGroup "Laws" $
    [ jsonLaws @Natural
    , jsonLaws @NatRatio
    , jsonLaws @Positive
    , jsonLaws @Rational
    , dataLaws @Natural
    , dataLaws @NatRatio
    , dataLaws @Positive
    , dataLaws @Rational
    , plutusEqLaws @Natural
    , plutusEqLawsSubstitution @Natural
    , plutusEqLaws @Positive
    , plutusEqLawsSubstitution @Positive
    , plutusEqLaws @Rational
    , plutusEqLawsSubstitution @Rational
    , plutusEqLaws @NatRatio
    , plutusEqLawsSubstitution @NatRatio
    , plutusOrdLaws @Natural
    , plutusOrdLaws @Positive
    , plutusOrdLaws @Rational
    , plutusOrdLaws @NatRatio
    , additiveSemigroupLaws @Natural
    , additiveSemigroupLaws @Positive
    , additiveSemigroupLaws @Rational
    , additiveSemigroupLaws @NatRatio
    , additiveMonoidLaws @Natural
    , additiveMonoidLaws @Rational
    , additiveMonoidLaws @NatRatio
    , additiveGroupLaws @Rational
    , multiplicativeSemigroupLaws @Natural
    , multiplicativeSemigroupLaws @Positive
    , multiplicativeSemigroupLaws @Rational
    , multiplicativeSemigroupLaws @NatRatio
    , multiplicativeMonoidLaws @Natural
    , multiplicativeMonoidLaws @Positive
    , multiplicativeMonoidLaws @Rational
    , multiplicativeMonoidLaws @NatRatio
    , semiringConsistencyLaws @Natural
    , semiringConsistencyLaws @Rational
    , semiringConsistencyLaws @NatRatio
    , laws @Natural "Additive hemigroup" additiveHemigroupLaws
    , laws @NatRatio "Additive hemigroup" additiveHemigroupLaws
    , laws @Natural "EuclideanClosed" euclideanClosedLaws
    , laws @Integer "EuclideanClosed" euclideanClosedSignedLaws
    , laws @Rational "Multiplicative group" multiplicativeGroupLaws
    , laws @NatRatio "Multiplicative group" multiplicativeGroupLaws
    , laws @Integer "IntegralDomain" integralDomainLaws
    , laws @Rational "IntegralDomain" integralDomainLaws
    ]
  where
    testMinimum :: QuickCheckTests
    testMinimum = 10000
