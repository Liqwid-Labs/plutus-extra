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

import Ledger.Typed.Scripts (DatumType, RedeemerType, ValidatorTypes)
import PlutusTx.IsData.Class (FromData, ToData)

--------------------------------------------------------------------------------
import PlutusTx.Data.Extra (fromDatum, fromRedeemer, toDatum, toRedeemer)

--------------------------------------------------------------------------------

data Test a
instance ValidatorTypes (Test a) where
  type RedeemerType (Test a) = a
  type DatumType (Test a) = a

prop_ToFromDatumRoundTrip ::
  forall (a :: Type).
  (ToData (DatumType (Test a)), FromData (DatumType (Test a)), Eq (DatumType (Test a))) =>
  DatumType (Test a) ->
  Bool
prop_ToFromDatumRoundTrip x = fromDatum @(Test a) (toDatum @(Test a) x) == Just x

prop_ToFromRedeemerRoundTrip ::
  forall (a :: Type).
  (ToData (RedeemerType (Test a)), FromData (RedeemerType (Test a)), Eq (RedeemerType (Test a))) =>
  RedeemerType (Test a) ->
  Bool
prop_ToFromRedeemerRoundTrip x = fromRedeemer @(Test a) (toRedeemer @(Test a) x) == Just x

-- Don't ask me.
pure []

tests :: [TestTree]
tests = [testProperties "Date.Extra properties" $allProperties]
