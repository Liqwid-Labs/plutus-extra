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
      ]
  ]
  where
    go :: QuickCheckTests
    go = 1_000_000

-- Helpers

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
