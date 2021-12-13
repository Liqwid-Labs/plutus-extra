{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Suites.PlutusTx.Data.Extra (tests) where

import Data.Kind (Type)
import Prelude

--------------------------------------------------------------------------------

import Test.Tasty
import Test.Tasty.QuickCheck (testProperties)

import Test.QuickCheck (allProperties)

--------------------------------------------------------------------------------

import PlutusTx.Data.Extra (fromDatum, fromRedeemer, toDatum, toRedeemer)
import PlutusTx.IsData.Class (FromData, ToData)

--------------------------------------------------------------------------------

prop_ToFromDatumRoundTrip ::
  forall (a :: Type).
  (ToData a, FromData a, Eq a) =>
  a ->
  Bool
prop_ToFromDatumRoundTrip x = fromDatum @a (toDatum @a x) == Just x

prop_ToFromRedeemerRoundTrip ::
  forall (a :: Type).
  (ToData a, FromData a, Eq a) =>
  a ->
  Bool
prop_ToFromRedeemerRoundTrip x = fromRedeemer @a (toRedeemer @a x) == Just x

-- Don't ask me.
pure []

tests :: [TestTree]
tests = [testProperties "Data.Extra properties" $allProperties]
