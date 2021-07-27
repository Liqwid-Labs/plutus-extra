{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}

module Suites.Bimap (tests) where

import Data.List (nub, sort)
import Prelude

--------------------------------------------------------------------------------

import Test.Tasty
import Test.Tasty.QuickCheck (testProperties)

import Test.QuickCheck (Arbitrary (arbitrary), allProperties)

--------------------------------------------------------------------------------

import PlutusTx.Ord qualified as PlutusTx

--------------------------------------------------------------------------------

import PlutusTx.Bimap (Bimap)
import PlutusTx.Bimap qualified as Bimap

--------------------------------------------------------------------------------

instance (PlutusTx.Ord a, Arbitrary a, PlutusTx.Ord b, Arbitrary b) => Arbitrary (Bimap a b) where
  arbitrary =
    Bimap.fromList <$> arbitrary @[(a, b)]

prop_InsertLookup a b m = b `elem` Bimap.lookup a (Bimap.insert a b m)

prop_InsertLookupR a b m = a `elem` Bimap.lookupR b (Bimap.insert a b m)

prop_ToListFromListSortedNub xs = Bimap.toList (Bimap.fromList xs) == sort (nub xs)

prop_InsertIdempotentLookup a b m = b `elem` Bimap.lookup a (Bimap.insert a b (Bimap.insert a b m))

prop_LengthPreservedNubbed xs = Bimap.size (Bimap.fromList xs) == toInteger (length (nub xs))

prop_NotNull a b m = not $ Bimap.null (Bimap.insert a b m)

-- Don't ask me.
pure []

tests :: [TestTree]
tests = [testProperties "Bimap properties" $allProperties]
