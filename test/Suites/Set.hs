{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}

module Suites.Set (tests) where

--------------------------------------------------------------------------------

import Data.List (nub)
import Prelude

--------------------------------------------------------------------------------

import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperties)

import Test.QuickCheck (Arbitrary (arbitrary), allProperties)

--------------------------------------------------------------------------------

import PlutusTx.Eq qualified as PlutusTx

--------------------------------------------------------------------------------

import PlutusTx.Set (Set)
import PlutusTx.Set qualified as Set

--------------------------------------------------------------------------------

instance (PlutusTx.Eq a, Arbitrary a) => Arbitrary (Set a) where
  arbitrary =
    Set.fromList <$> arbitrary @[a]

prop_InsertionEmpty :: (Eq a, PlutusTx.Eq a) => a -> Bool
prop_InsertionEmpty x = Set.insert x Set.empty == Set.singleton x

prop_IdempotentInsert :: (Eq a, PlutusTx.Eq a) => a -> Set a -> Bool
prop_IdempotentInsert x xs = Set.insert x (Set.insert x xs) == Set.insert x xs

prop_ToListFromListNub :: (Eq a, PlutusTx.Eq a) => [a] -> Bool
prop_ToListFromListNub xs = Set.toList (Set.fromList xs) == nub xs

prop_LengthPreservedNubbed :: (PlutusTx.Eq a, Eq a) => [a] -> Bool
prop_LengthPreservedNubbed xs = Set.size (Set.fromList xs) == toInteger (length (nub xs))

prop_UnionNubbedConcat :: (Eq a, PlutusTx.Eq a) => [a] -> [a] -> Bool
prop_UnionNubbedConcat xs ys =
  Set.toList (Set.fromList xs `Set.union` Set.fromList ys)
    == nub (xs <> ys)

prop_InsertDeleteDelete :: (Eq a, PlutusTx.Eq a) => a -> Set a -> Bool
prop_InsertDeleteDelete x xs = Set.delete x (Set.insert x xs) == Set.delete x xs

prop_NotNull :: PlutusTx.Eq a => a -> Set a -> Bool
prop_NotNull x xs = not $ Set.null (Set.insert x xs)

prop_ElemsAlwaysUnique :: Eq a => Set a -> Bool
prop_ElemsAlwaysUnique xs = Set.toList xs == nub (Set.toList xs)

-- Don't ask me.
pure []

tests :: [TestTree]
tests = [testProperties "Set properties" $allProperties]
