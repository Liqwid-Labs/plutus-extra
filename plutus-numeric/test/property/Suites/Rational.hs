{-# LANGUAGE MultiWayIf #-}

module Suites.Rational (tests) where

import Control.Monad (guard)
import Data.Bifunctor (bimap)
import PlutusTx.Prelude qualified as PTx
import PlutusTx.Rational qualified as Rational
import Test.QuickCheck (
  Property,
  checkCoverage,
  cover,
  coverTable,
  forAllShrinkShow,
  property,
  tabulate,
  (===),
 )
import Test.QuickCheck.Arbitrary (arbitrary, shrink)
import Test.QuickCheck.Gen (Gen, elements, oneof, suchThat)
import Test.QuickCheck.Modifiers (
  Negative (Negative),
  NonZero (NonZero),
  Positive (Positive),
 )
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)
import Text.Show.Pretty (ppShow)

tests :: [TestTree]
tests =
  [ localOption go . testGroup "%" $
      [ testProperty
          ( "Signs of numerator and denominator "
              <> "determine sign of rational"
          )
          signProp
      , testProperty
          ( "Relative absolute values "
              <> "of numerator and denominator "
              <> "determine value of rational"
          )
          absValProp
      , testProperty "0 % x normalizes" zeroNormalProp
      , testProperty "x % x normalizes" oneNormalProp
      , testProperty "x % y = (x * z) % (y * z)" scaleNormalizationProp
      ]
  , localOption go . testGroup "fromInteger" $
      [ testProperty "fromInteger x = x % 1" fromIntegerProp
      ]
  , localOption go . testGroup "numerator and denominator" $
      [ testProperty
          "numerator r = numerator (r * (fromInteger . denominator $ r))"
          numDenRelationProp
      , testProperty "denominator r > 0" positiveDenominatorProp
      ]
  , localOption go . testGroup "recip" $
      [ testProperty "recip (x % y) = y % x" recipInversionProp
      , testProperty "recip r * r = 1" recipMultiplicationProp
      ]
  , localOption go . testGroup "abs" $
      [ testProperty "abs r >= 0" absNonNegativeProp
      , testProperty "abs n % abs d = abs (n % d)" absBuildProp
      , testProperty "abs r * abs r' = abs (r * r')" absMultProp
      ]
  ]
  where
    go :: QuickCheckTests
    go = 1_000_000

-- TODO: round, truncate, properFraction

-- Helpers

absNonNegativeProp :: Property
absNonNegativeProp = forAllShrinkShow arbitrary shrink ppShow go
  where
    go :: Rational.Rational -> Property
    go r = property (Rational.abs r PTx.>= PTx.zero)

absBuildProp :: Property
absBuildProp = forAllShrinkShow arbitrary shrink ppShow go
  where
    go :: (Integer, NonZero Integer) -> Property
    go (n, NonZero d) =
      abs n Rational.% abs d === Rational.abs (n Rational.% d)

absMultProp :: Property
absMultProp = forAllShrinkShow arbitrary shrink ppShow go
  where
    go :: (Rational.Rational, Rational.Rational) -> Property
    go (r, r') =
      Rational.abs r PTx.* Rational.abs r' === Rational.abs (r PTx.* r')

recipMultiplicationProp :: Property
recipMultiplicationProp = forAllShrinkShow gen shr ppShow go
  where
    gen :: Gen Rational.Rational
    gen = suchThat arbitrary (PTx./= PTx.zero)
    shr :: Rational.Rational -> [Rational.Rational]
    shr r = do
      r' <- shrink r
      guard (r' PTx./= PTx.zero)
      pure r'
    go :: Rational.Rational -> Property
    go r = Rational.recip r PTx.* r === PTx.one

recipInversionProp :: Property
recipInversionProp = forAllShrinkShow arbitrary shrink ppShow go
  where
    go :: (NonZero Integer, NonZero Integer) -> Property
    go (NonZero n, NonZero d) =
      Rational.recip (n Rational.% d) === d Rational.% n

numDenRelationProp :: Property
numDenRelationProp = forAllShrinkShow arbitrary shrink ppShow go
  where
    go :: Rational.Rational -> Property
    go r =
      Rational.numerator r
        === (Rational.numerator . (r PTx.*) . Rational.fromInteger . Rational.denominator $ r)

positiveDenominatorProp :: Property
positiveDenominatorProp = forAllShrinkShow arbitrary shrink ppShow go
  where
    go :: Rational.Rational -> Property
    go r = property (Rational.denominator r PTx.> PTx.zero)

fromIntegerProp :: Property
fromIntegerProp = forAllShrinkShow arbitrary shrink ppShow go
  where
    go :: Integer -> Property
    go i = Rational.fromInteger i === i Rational.% PTx.one

zeroNormalProp :: Property
zeroNormalProp = forAllShrinkShow arbitrary shrink ppShow go
  where
    go :: NonZero Integer -> Property
    go (NonZero i) = PTx.zero Rational.% i === PTx.zero

oneNormalProp :: Property
oneNormalProp = forAllShrinkShow arbitrary shrink ppShow go
  where
    go :: NonZero Integer -> Property
    go (NonZero i) = i Rational.% i === PTx.one

scaleNormalizationProp :: Property
scaleNormalizationProp = forAllShrinkShow arbitrary shrink ppShow go
  where
    go :: (Integer, NonZero Integer, NonZero Integer) -> Property
    go (x, NonZero y, NonZero z) =
      x Rational.% y === (x PTx.* z) Rational.% (y PTx.* z)

absValProp :: Property
absValProp = forAllShrinkShow gen shr ppShow go
  where
    gen :: Gen (Integer, Integer)
    gen = oneof [signsMatch, signsDiffer]
    signsMatch :: Gen (Integer, Integer)
    signsMatch = do
      mk <- elements [mkPos, mkNeg]
      (n, d) <- suchThat ((,) <$> mk <*> mk) (uncurry (/=) . bimap abs abs)
      pure (n, d)
    mkPos :: Gen Integer
    mkPos = do
      Positive x <- arbitrary
      pure x
    mkNeg :: Gen Integer
    mkNeg = do
      Negative x <- arbitrary
      pure x
    signsDiffer :: Gen (Integer, Integer)
    signsDiffer = do
      (genN, genD) <- elements [(mkPos, mkNeg), (mkNeg, mkPos)]
      (n, d) <- suchThat ((,) <$> genN <*> genD) (uncurry (/=) . bimap abs abs)
      pure (n, d)
    shr :: (Integer, Integer) -> [(Integer, Integer)]
    shr (n, d) = do
      n' <- case signum n of
        (-1) -> do
          Negative n' <- shrink . Negative $ n
          pure n'
        _ -> do
          Positive n' <- shrink . Positive $ n
          pure n'
      guard (abs n' /= abs d)
      pure (n', d)
    go :: (Integer, Integer) -> Property
    go (n, d) =
      checkCoverage
        . cover 50.0 (sameSign n d) "signs match"
        $ if signum n == signum d
          then checkSignMatch n d
          else checkSignDiffer n d
    sameSign :: Integer -> Integer -> Bool
    sameSign x y = signum x == signum y
    checkSignMatch :: Integer -> Integer -> Property
    checkSignMatch n d =
      let absN = abs n
          absD = abs d
          r = n Rational.% d
       in if absN > absD
            then property (r PTx.> PTx.one)
            else property (r PTx.< PTx.one)
    checkSignDiffer :: Integer -> Integer -> Property
    checkSignDiffer n d =
      let absN = abs n
          absD = abs d
          r = n Rational.% d
       in if absN > absD
            then property (r PTx.< PTx.negate PTx.one)
            else property (r PTx.> PTx.negate PTx.one)

signProp :: Property
signProp = forAllShrinkShow gen shr ppShow go
  where
    gen :: Gen (Integer, NonZero Integer)
    gen = oneof [zeroNum, sameSign, diffSign]
    zeroNum :: Gen (Integer, NonZero Integer)
    zeroNum = (0,) <$> arbitrary
    sameSign :: Gen (Integer, NonZero Integer)
    sameSign = oneof [bothPos, bothNeg]
    bothPos :: Gen (Integer, NonZero Integer)
    bothPos = do
      Positive n <- arbitrary
      Positive d <- arbitrary
      pure (n, NonZero d)
    bothNeg :: Gen (Integer, NonZero Integer)
    bothNeg = do
      Negative n <- arbitrary
      Negative d <- arbitrary
      pure (n, NonZero d)
    diffSign :: Gen (Integer, NonZero Integer)
    diffSign = oneof [posNeg, negPos]
    posNeg :: Gen (Integer, NonZero Integer)
    posNeg = do
      Positive n <- arbitrary
      Negative d <- arbitrary
      pure (n, NonZero d)
    negPos :: Gen (Integer, NonZero Integer)
    negPos = do
      Negative n <- arbitrary
      Positive d <- arbitrary
      pure (n, NonZero d)
    shr :: (Integer, NonZero Integer) -> [(Integer, NonZero Integer)]
    shr (n, NonZero d)
      | n == 0 = (0,) <$> (shrink . NonZero $ d)
      | otherwise = case signum n of
        (-1) -> do
          Negative n' <- shrink . Negative $ n
          pure (n', NonZero d)
        _ -> do
          Positive n' <- shrink . Positive $ n
          pure (n', NonZero d)
    go :: (Integer, NonZero Integer) -> Property
    go (n, NonZero d) =
      checkCoverage
        . coverTable "Cases" caseTable
        . tabulate "Cases" [nameCase n d]
        $ if
            | signum n == 0 -> n Rational.% d === PTx.zero
            | signum n == signum d -> property (n Rational.% d PTx.> PTx.zero)
            | otherwise -> property (n Rational.% d PTx.< PTx.zero)
    nameCase :: Integer -> Integer -> String
    nameCase n d
      | signum n == 0 = "zero numerator"
      | signum n == signum d = "same signs"
      | otherwise = "different signs"
    caseTable :: [(String, Double)]
    caseTable =
      [ ("zero numerator", 33.3)
      , ("same signs", 33.3)
      , ("different signs", 33.3)
      ]
