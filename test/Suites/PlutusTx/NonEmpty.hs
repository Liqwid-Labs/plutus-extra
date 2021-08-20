{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}

module Suites.PlutusTx.NonEmpty (tests) where

--------------------------------------------------------------------------------

import PlutusTx.Prelude qualified as PlutusPrelude
import Prelude hiding (Eq)

--------------------------------------------------------------------------------

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

--------------------------------------------------------------------------------

import PlutusTx.IsData.Class (
  FromData (fromBuiltinData),
  ToData (toBuiltinData),
  UnsafeFromData (unsafeFromBuiltinData),
 )
import PlutusTx.NonEmpty (NonEmpty ((:|)))
import PlutusTx.NonEmpty qualified as NonEmpty

--------------------------------------------------------------------------------

instance (PlutusPrelude.Eq a, Arbitrary a) => Arbitrary (NonEmpty a) where
  arbitrary =
    (:|) <$> arbitrary @a <*> arbitrary @[a]

prop_Insertion x xs = NonEmpty.toList (x :| xs) == x : xs

prop_HeadTailConcat xs = NonEmpty.head xs :| NonEmpty.tail xs PlutusPrelude.== xs

prop_InitLastReverseConcat xs =
  NonEmpty.last xs :| reverse (NonEmpty.init xs) PlutusPrelude.== NonEmpty.reverse xs

prop_ToFromDataRoundTrip (xs :: NonEmpty a) =
  fromBuiltinData (toBuiltinData xs) PlutusPrelude.== Just xs

prop_ToUnsafeFromDataRoundTrip (xs :: NonEmpty a) =
  unsafeFromBuiltinData (toBuiltinData xs) PlutusPrelude.== xs

-- Don't ask me.
pure []

tests :: [TestTree]
tests = [testProperties "NonEmpty properties" $allProperties]
