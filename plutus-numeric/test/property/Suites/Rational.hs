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
  (.||.),
  (===),
 )
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink))
import Test.QuickCheck.Gen (
  Gen,
  chooseInteger,
  elements,
  oneof,
  suchThat,
 )
import Test.QuickCheck.Modifiers (
  Negative (Negative),
  NonNegative (NonNegative),
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
  , localOption go . testGroup "properFraction" $
      [ testProperty "r = n + f, where (n, f) = properFraction r" pfInvertProp
      , testProperty "signs of components match sign of input" pfSignProp
      , testProperty "abs f < 1, where (_, f) = properFraction r" pfAbsProp
      ]
  , localOption go . testGroup "truncate" $
      [ testProperty "truncate r = n where (n, _) = properFraction r" truncateProp
      ]
  , localOption go . testProperty "round" $ roundProp
  ]
  where
    go :: QuickCheckTests
    go = 1_000_000

-- Helpers

roundProp :: Property
roundProp = forAllShrinkShow gen shr ppShow go
  where
    gen :: Gen (Integer, Rational.Rational)
    gen = oneof [minusOne, matching, plusOne]
    minusOne :: Gen (Integer, Rational.Rational)
    minusOne = oneof [eqNegOdd, gtNeg]
    eqNegOdd :: Gen (Integer, Rational.Rational)
    eqNegOdd = do
      Odd n <- arbitrary
      pure (negate . abs $ n, Rational.negate Rational.half)
    gtNeg :: Gen (Integer, Rational.Rational)
    gtNeg = do
      Negative n <- arbitrary
      Positive den <- arbitrary
      let split = den `quot` 2
      num <- chooseInteger (split + 1, den - 1)
      pure (negate n, negate num Rational.% den)
    matching :: Gen (Integer, Rational.Rational)
    matching = oneof [ltAny, eqEven]
    ltAny :: Gen (Integer, Rational.Rational)
    ltAny = do
      n <- arbitrary
      Positive den <- arbitrary
      num <- chooseInteger (0, den - 1)
      let f = case signum n of
            (-1) -> negate num Rational.% den
            _ -> num Rational.% den
      pure (n, f)
    eqEven :: Gen (Integer, Rational.Rational)
    eqEven = do
      Even n <- arbitrary
      f <- case signum n of
        0 -> elements [Rational.half, Rational.negate Rational.half]
        (-1) -> pure . Rational.negate $ Rational.half
        _ -> pure Rational.half
      pure (n, f)
    plusOne :: Gen (Integer, Rational.Rational)
    plusOne = oneof [eqPosOdd, gtNonNeg]
    eqPosOdd :: Gen (Integer, Rational.Rational)
    eqPosOdd = do
      Odd n <- arbitrary
      pure (abs n, Rational.half)
    gtNonNeg :: Gen (Integer, Rational.Rational)
    gtNonNeg = do
      NonNegative n <- arbitrary
      Positive den <- arbitrary
      let split = den `quot` 2
      num <- chooseInteger (split + 1, den - 1)
      pure (n, num Rational.% den)
    shr :: (Integer, Rational.Rational) -> [(Integer, Rational.Rational)]
    shr = const [] -- not even going to _try_ shrinking this
    go :: (Integer, Rational.Rational) -> Property
    go (n, f) =
      let r = Rational.fromInteger n PTx.+ f
          rounded = Rational.round r
          f' = Rational.abs f
       in checkCoverage
            . coverTable "Cases" caseTable
            . tabulate "Cases" [nameCase n f']
            $ if
                | f' PTx.< Rational.half -> rounded === (n - 1)
                | f' PTx.> Rational.half && signum n == (-1) -> rounded === (n - 1)
                | f' PTx.> Rational.half -> rounded === (n + 1)
                | f' PTx.== Rational.half && odd n -> case signum n of
                  (-1) -> rounded === (n - 1)
                  _ -> rounded === (n + 1)
                | otherwise -> rounded === n
    caseTable :: [(String, Double)]
    caseTable =
      [ ("n - 1", 33.3)
      , ("n", 33.3)
      , ("n + 1", 33.3)
      ]
    nameCase :: Integer -> Rational.Rational -> String
    nameCase n f
      | f PTx.< Rational.half = "n - 1"
      | f PTx.> Rational.half && signum n == (-1) = "n - 1"
      | f PTx.> Rational.half = "n + 1"
      | f PTx.== Rational.half && odd n = case signum n of
        (-1) -> "n - 1"
        _ -> "n + 1"
      | otherwise = "n"

truncateProp :: Property
truncateProp = forAllShrinkShow arbitrary shrink ppShow go
  where
    go :: Rational.Rational -> Property
    go r =
      let (n, _) = Rational.properFraction r
       in n === Rational.truncate r

pfAbsProp :: Property
pfAbsProp = forAllShrinkShow arbitrary shrink ppShow go
  where
    go :: Rational.Rational -> Property
    go r =
      let (_, f) = Rational.properFraction r
       in property (Rational.abs f PTx.< PTx.one)

pfSignProp :: Property
pfSignProp = forAllShrinkShow gen shr ppShow go
  where
    gen :: Gen Rational.Rational
    gen = suchThat arbitrary (PTx./= PTx.zero)
    shr :: Rational.Rational -> [Rational.Rational]
    shr r = do
      r' <- shrink r
      guard (r' /= PTx.zero)
      pure r'
    go :: Rational.Rational -> Property
    go r =
      let (n, d) = Rational.properFraction r
       in if r PTx.> PTx.zero
            then (n PTx.> PTx.zero) .||. (d PTx.> PTx.zero)
            else (n PTx.< PTx.zero) .||. (d PTx.< PTx.zero)

pfInvertProp :: Property
pfInvertProp = forAllShrinkShow arbitrary shrink ppShow go
  where
    go :: Rational.Rational -> Property
    go r =
      let (n, f) = Rational.properFraction r
          n' = Rational.fromInteger n
       in n' PTx.+ f === r

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

newtype Odd = Odd Integer
  deriving stock (Eq, Show)

instance Arbitrary Odd where
  arbitrary = Odd . (+ 1) . (* 2) <$> arbitrary
  shrink (Odd n) = do
    n' <- shrink n
    guard (odd n')
    pure . Odd $ n'

newtype Even = Even Integer
  deriving stock (Eq, Show)

instance Arbitrary Even where
  arbitrary = Even . (* 2) <$> arbitrary
  shrink (Even n) = do
    n' <- shrink n
    guard (even n')
    pure . Even $ n'
