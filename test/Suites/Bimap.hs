{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}

module Suites.Bimap (tests) where

import Data.List (nub)
import Prelude

--------------------------------------------------------------------------------

import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperties)

import Test.QuickCheck (Arbitrary (arbitrary), allProperties)

--------------------------------------------------------------------------------

import PlutusTx.Eq qualified as PlutusTx

--------------------------------------------------------------------------------

import PlutusTx.Bimap (Bimap)
import PlutusTx.Bimap qualified as Bimap

--------------------------------------------------------------------------------

instance (PlutusTx.Eq a, Arbitrary a, PlutusTx.Eq b, Arbitrary b) => Arbitrary (Bimap a b) where
  arbitrary =
    Bimap.fromList <$> arbitrary @[(a, b)]

prop_InsertLookup :: (Eq b, PlutusTx.Eq a, PlutusTx.Eq b) => a -> b -> Bimap a b -> Bool
prop_InsertLookup a b m = b `elem` Bimap.lookup a (Bimap.insert a b m)

prop_InsertLookupR :: (Eq a, PlutusTx.Eq b, PlutusTx.Eq a) => a -> b -> Bimap a b -> Bool
prop_InsertLookupR a b m = a `elem` Bimap.lookupR b (Bimap.insert a b m)

prop_ToListFromListNub :: (Eq a, Eq b, PlutusTx.Eq a, PlutusTx.Eq b) => [(a, b)] -> Bool
prop_ToListFromListNub xs = Bimap.toList (Bimap.fromList xs) == nub xs

prop_InsertIdempotentLookup :: (Eq b, PlutusTx.Eq a, PlutusTx.Eq b) => a -> b -> Bimap a b -> Bool
prop_InsertIdempotentLookup a b m = b `elem` Bimap.lookup a (Bimap.insert a b (Bimap.insert a b m))

prop_LengthPreservedNubbed :: (PlutusTx.Eq a, PlutusTx.Eq b, Eq a, Eq b) => [(a, b)] -> Bool
prop_LengthPreservedNubbed xs = Bimap.size (Bimap.fromList xs) == toInteger (length (nub xs))

prop_NotNull :: (PlutusTx.Eq a, PlutusTx.Eq b) => a -> b -> Bimap a b -> Bool
prop_NotNull a b m = not $ Bimap.null (Bimap.insert a b m)

-- Don't ask me.
pure []

tests :: [TestTree]
tests = [testProperties "Bimap properties" $allProperties]
