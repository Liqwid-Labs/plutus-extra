{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}

module Suites.Set (tests) where

--------------------------------------------------------------------------------

import Data.List (nub, sort)
import Prelude

--------------------------------------------------------------------------------

import Test.Tasty
import Test.Tasty.QuickCheck

import Test.QuickCheck

--------------------------------------------------------------------------------

import PlutusTx.Ord qualified as PlutusTx

--------------------------------------------------------------------------------

import PlutusTx.IsData.Class (
  FromData (fromBuiltinData),
  ToData (toBuiltinData),
  UnsafeFromData (unsafeFromBuiltinData),
 )
import PlutusTx.Set (Set)
import PlutusTx.Set qualified as Set

--------------------------------------------------------------------------------

instance (PlutusTx.Ord a, Arbitrary a) => Arbitrary (Set a) where
  arbitrary =
    Set.fromList <$> arbitrary @[a]

prop_InsertionEmpty x = Set.insert x Set.empty == Set.singleton x

prop_IdempotentInsert x xs = Set.insert x (Set.insert x xs) == Set.insert x xs

prop_ToListFromListSortedNub xs = Set.toList (Set.fromList xs) == sort (nub xs)

prop_LengthPreservedNubbed xs = Set.size (Set.fromList xs) == toInteger (length (nub xs))

prop_UnionSortedNubbedConcat xs ys =
  Set.toList (Set.fromList xs `Set.union` Set.fromList ys)
    == sort (nub (xs <> ys))

prop_InsertDeleteDelete x xs = Set.delete x (Set.insert x xs) == Set.delete x xs

prop_NotNull x xs = not $ Set.null (Set.insert x xs)

prop_ElemsAlwaysUnique xs = Set.toList xs == nub (Set.toList xs)

prop_ToFromDataRoundTrip (xs :: Set a) = fromBuiltinData (toBuiltinData xs) == Just xs

prop_ToUnsafeFromDataRoundTrip (xs :: Set a) = unsafeFromBuiltinData (toBuiltinData xs) == xs

-- Don't ask me.
pure []

tests :: [TestTree]
tests = [testProperties "Set properties" $allProperties]
