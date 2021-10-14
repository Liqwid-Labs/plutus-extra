{-# OPTIONS_GHC -Wno-orphans #-}

module Helpers (
  NonZero (..),
  NonNegative (..)) where

import PlutusTx.Ratio qualified as Ratio
import Data.Kind (Type)
import PlutusTx.Prelude qualified as PlutusPrelude
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink))
import Test.QuickCheck.Gen (suchThat)

-- A newtype which ensures we generate a value that is not zero.
newtype NonZero (a :: Type) = NonZero a
  deriving stock (Show)
  deriving
    ( Eq
    , PlutusPrelude.AdditiveSemigroup
    , PlutusPrelude.AdditiveMonoid
    )
    via a

instance
  (Eq a, PlutusPrelude.AdditiveMonoid a, Arbitrary a) =>
  Arbitrary (NonZero a)
  where
  arbitrary = do
    x <- arbitrary `suchThat` (PlutusPrelude.zero /=)
    pure . NonZero $ x
  shrink (NonZero x) = do
    x' <- filter (PlutusPrelude.zero /=) . shrink $ x
    pure . NonZero $ x'

-- A newtype which ensures we generate a value that's not less than zero.
newtype NonNegative (a :: Type) = NonNegative a
  deriving stock (Show)
  deriving (Eq) via a

instance
  (Arbitrary a, PlutusPrelude.AdditiveMonoid a, PlutusPrelude.Ord a) =>
  Arbitrary (NonNegative a)
  where
  arbitrary = do
    x <- arbitrary `suchThat` (PlutusPrelude.zero PlutusPrelude.<=)
    pure . NonNegative $ x
  shrink (NonNegative x) = do
    x' <- filter (PlutusPrelude.zero PlutusPrelude.<=) . shrink $ x
    pure . NonNegative $ x'

-- Needed due to a Plutus deficiency
instance Arbitrary Ratio.Rational where
  arbitrary = Ratio.fromGHC <$> arbitrary
  shrink = fmap Ratio.fromGHC . shrink . Ratio.toGHC
