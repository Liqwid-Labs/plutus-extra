{-# LANGUAGE AllowAmbiguousTypes #-}

module Suites.Numeric (tests) where

import Data.Kind (Type)
import Helpers (
  NonNegative (NonNegative),
  NonZero (NonZero),
 )
import PlutusTx.Numeric.Extra (
  IntegralDomain (
    abs,
    addExtend,
    projectAbs,
    restrictMay,
    signum
  ),
  reciprocal,
  (/),
  (^),
 )
import PlutusTx.Prelude qualified as Plutus
import PlutusTx.Ratio (Rational)
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Property,
  forAllShrink,
  (=/=),
  (===),
 )
import Test.QuickCheck.Modifiers (Negative (Negative), Positive (Positive))
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)
import Prelude hiding (Rational, abs, divMod, signum, (/), (^))

tests :: [TestTree]
tests =
  [ localOption go . testGroup "MultiplicativeGroup, Rational" $
      [ testProperty "if x / y = z, then y * z = x" mgProp1
      , testProperty "x / y = x * reciprocal y" mgProp2
      , testProperty "x ^ 0 = 1" mgProp3
      , testProperty "x ^ 1 = x" mgProp4
      , localOption goSmall
          . testProperty "If i < 0, x /= 0, x ^ i = recip (x ^ |i|)"
          $ mgProp5
      , localOption goSmall
          . testProperty "If i > 1, then x ^ i = x * x ^ (i - 1)"
          $ mgProp6
      ]
  , localOption go . testGroup "IntegralDomain, Integer" $
      [ testProperty "abs x >= 0" (idProp1 @Integer)
      , testCase "abs 0 = 0" . assertEqual "" (Plutus.zero :: Integer) $ abs Plutus.zero
      , testProperty "abs (x * y) = abs x * abs y" (idProp2 @Integer)
      , testProperty "abs (x + y) <= abs x + abs y" (idProp3 @Integer)
      , testProperty "abs x * signum x = x" (idProp4 @Integer)
      , testProperty "projectAbs . addExtend = id" (idProp5 @Integer)
      , testProperty "If abs x = x, then addExtend . projectAbs $ x = x" (idProp6 @Integer)
      , testProperty "restrictMay x = Just y <-> abs x = x" (idProp7 @Integer)
      ]
  , localOption go . testGroup "IntegralDomain, Rational" $
      [ testProperty "abs x >= 0" (idProp1 @Rational)
      , testCase "abs 0 = 0" . assertEqual "" (Plutus.zero :: Rational) $ abs Plutus.zero
      , testProperty "abs (x * y) = abs x * abs y" (idProp2 @Rational)
      , testProperty "abs (x + y) <= abs x + abs y" (idProp3 @Rational)
      , testProperty "abs x * signum x = x" (idProp4 @Rational)
      , testProperty "projectAbs . addExtend = id" (idProp5 @Rational)
      , testProperty "If abs x = x, then addExtend . projectAbs $ x = x" (idProp6 @Rational)
      , testProperty "restrictMay x = Just y <-> abs x = x" (idProp7 @Rational)
      ]
  ]
  where
    go :: QuickCheckTests
    go = 1000000
    goSmall :: QuickCheckTests
    goSmall = 10000

idProp1 ::
  forall (a :: Type) (r :: Type).
  (Show a, Arbitrary a, IntegralDomain a r) =>
  Property
idProp1 = forAllShrink arbitrary shrink go
  where
    go :: a -> Property
    go x = Plutus.compare (abs x) Plutus.zero =/= LT

idProp2 ::
  forall (a :: Type) (r :: Type).
  (Arbitrary a, IntegralDomain a r, Eq a, Show a) =>
  Property
idProp2 = forAllShrink arbitrary shrink go
  where
    go :: (a, a) -> Property
    go (x, y) = abs (x Plutus.* y) === abs x Plutus.* abs y

idProp3 ::
  forall (a :: Type) (r :: Type).
  (Show a, Arbitrary a, IntegralDomain a r) =>
  Property
idProp3 = forAllShrink arbitrary shrink go
  where
    go :: (a, a) -> Property
    go (x, y) =
      let lhs = abs (x Plutus.+ y)
          rhs = abs x Plutus.+ abs y
       in Plutus.compare lhs rhs =/= GT

idProp4 ::
  forall (a :: Type) (r :: Type).
  (Arbitrary a, IntegralDomain a r, Eq a, Show a) =>
  Property
idProp4 = forAllShrink arbitrary shrink go
  where
    go :: a -> Property
    go x = abs x Plutus.* signum x === x

idProp5 ::
  forall (a :: Type) (r :: Type).
  (Arbitrary r, Eq r, IntegralDomain a r, Show r) =>
  Property
idProp5 = forAllShrink arbitrary shrink go
  where
    go :: r -> Property
    go x = (projectAbs . addExtend $ x) === x

idProp6 ::
  forall (a :: Type) (r :: Type).
  (Arbitrary a, Eq a, IntegralDomain a r, Show a) =>
  Property
idProp6 = forAllShrink arbitrary shrink go
  where
    go :: NonNegative a -> Property
    go (NonNegative x) = (addExtend . projectAbs $ x) === x

idProp7 ::
  forall (a :: Type) (r :: Type).
  (Show a, Arbitrary a, IntegralDomain a r, Eq a) =>
  Property
idProp7 = forAllShrink arbitrary shrink go
  where
    go :: a -> Property
    go x = case restrictMay x of
      Nothing -> abs x =/= x
      Just _ -> abs x === x

mgProp1 :: Property
mgProp1 = forAllShrink arbitrary shrink go
  where
    go :: (Rational, NonZero Rational) -> Property
    go (x, NonZero y) =
      let z = x / y
       in x === y Plutus.* z

mgProp2 :: Property
mgProp2 = forAllShrink arbitrary shrink go
  where
    go :: (Rational, NonZero Rational) -> Property
    go (x, NonZero y) = x / y === x Plutus.* reciprocal y

mgProp3 :: Property
mgProp3 = forAllShrink arbitrary shrink go
  where
    go :: Rational -> Property
    go x = x ^ Plutus.zero === Plutus.one

mgProp4 :: Property
mgProp4 = forAllShrink arbitrary shrink go
  where
    go :: Rational -> Property
    go x = x ^ Plutus.one === x

mgProp5 :: Property
mgProp5 = forAllShrink arbitrary shrink go
  where
    go :: (NonZero Rational, Negative Integer) -> Property
    go (NonZero x, Negative i) =
      x ^ i === reciprocal (x ^ abs i)

mgProp6 :: Property
mgProp6 = forAllShrink arbitrary shrink go
  where
    go :: (Rational, Positive Integer) -> Property
    go (x, Positive i) =
      x ^ i === x Plutus.* (x ^ (i - 1))
