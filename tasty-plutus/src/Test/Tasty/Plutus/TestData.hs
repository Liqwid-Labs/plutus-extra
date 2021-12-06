{-# LANGUAGE Trustworthy #-}

{- |
 Module: Test.Tasty.Plutus.TestData
 Copyright: (C) MLabs 2021
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental
-}
module Test.Tasty.Plutus.TestData (
  -- * Data type
  TestData (..),

  -- * QuickCheck support
  Example (..),
  Methodology (..),
  fromArbitrary,
  static,
  Generator (..),
  TestItems (..),
) where

import Data.Kind (Type)
import Data.Semigroup (stimes, stimesIdempotent)
import Plutus.V1.Ledger.Value (Value)
import PlutusTx.IsData.Class (FromData, ToData)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink))
import Test.QuickCheck.Gen (Gen)
import Test.Tasty.Plutus.Context (ContextBuilder)
import Test.Tasty.Plutus.Internal.Context (
  Purpose (ForMinting, ForSpending),
 )
import Prelude

{- | All the data needed to test a validator or minting policy.

 @since 3.0
-}
data TestData (p :: Purpose) where
  -- | @since 3.0
  SpendingTest ::
    ( ToData datum
    , ToData redeemer
    , FromData datum
    , FromData redeemer
    , Show datum
    , Show redeemer
    ) =>
    -- | The input datum.
    --
    -- @since 3.0
    datum ->
    -- | The input redeemer.
    --
    -- @since 3.0
    redeemer ->
    -- | The value to be spent from the script.
    --
    -- @since 3.0
    Value ->
    TestData 'ForSpending
  -- | @since 3.0
  MintingTest ::
    (ToData redeemer, FromData redeemer, Show redeemer) =>
    redeemer ->
    TestData 'ForMinting

{- | Describes whether a case is good (i.e. should pass) or bad (i.e. should
 fail). Used to classify generated outputs for QuickCheck-based tests.

 @since 3.1
-}
data Example = Good | Bad
  deriving stock
    ( -- | @since 3.1
      Eq
    , -- | @since 3.1
      Show
    )

{- | \'Maximal badness\': gives 'Bad' when any argument is 'Bad'.

 @since 3.1
-}
instance Semigroup Example where
  {-# INLINEABLE (<>) #-}
  Bad <> _ = Bad
  _ <> Bad = Bad
  _ <> _ = Good
  {-# INLINEABLE stimes #-}
  stimes = stimesIdempotent

{- | A reification of 'Arbitrary'. Consists of a combination of generator and
 shrinker.

 @since 3.1
-}
data Methodology (a :: Type) = Methodology (Gen a) (a -> [a])

{- | \'Capture\' an existing 'Arbitrary' instance as a 'Methodology'.

 @since 3.1
-}
fromArbitrary ::
  forall (a :: Type).
  (Arbitrary a) =>
  Methodology a
fromArbitrary = Methodology arbitrary shrink

{- | Always generates the same value, and never shrinks it.

 @since 3.1
-}
static ::
  forall (a :: Type).
  a ->
  Methodology a
static x = Methodology (pure x) (const [])

{- | Contains a means of generating a seed and a function
 for creating from seed 'TestData', 'ContextBuilder'
 as well as a way to determine a \'good\' or \'bad\' generated case.

 @since 5.0
-}
data Generator (a :: Type) (p :: Purpose) where
  -- | @since 5.0
  GenForSpending ::
    (Show a) =>
    Methodology a ->
    (a -> TestItems 'ForSpending) ->
    Generator a 'ForSpending
  -- | @since 5.0
  GenForMinting ::
    (Show a) =>
    Methodology a ->
    (a -> TestItems 'ForMinting) ->
    Generator a 'ForMinting

data TestItems (p :: Purpose) where
  -- | @since 5.0
  ItemsForSpending ::
    forall (datum :: Type) (redeemer :: Type).
    ( ToData datum
    , ToData redeemer
    , FromData datum
    , FromData redeemer
    , Show datum
    , Show redeemer
    ) =>
    { spendDatum :: datum
    , spendRedeemer :: redeemer
    , spendValue :: Value
    , spendCB :: ContextBuilder 'ForSpending
    , spendExample :: Example
    } ->
    TestItems 'ForSpending
  -- | @since 5.0
  ItemsForMinting ::
    forall (redeemer :: Type).
    ( ToData redeemer
    , FromData redeemer
    , Show redeemer
    ) =>
    { mintRedeemer :: redeemer
    , mintCB :: ContextBuilder 'ForMinting
    , mintExample :: Example
    } ->
    TestItems 'ForMinting
