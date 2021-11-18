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
  fromArbitrarySpending,
  fromArbitraryMinting,
) where

import Data.Kind (Type)
import Data.Semigroup (stimes, stimesIdempotent)
import Plutus.V1.Ledger.Value (Value)
import PlutusTx.IsData.Class (FromData, ToData)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink))
import Test.QuickCheck.Gen (Gen)
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
  MintingTest ::
    (ToData redeemer, FromData redeemer, Show redeemer) =>
    redeemer ->
    -- @since 4.1
    Value ->
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

{- | Contains a means of generating a 'TestData', as well as a way to determine
 a \'good\' or \'bad\' generated case.

 @since 3.1
-}
data Generator (p :: Purpose) where
  -- | @since 3.1
  GenForSpending ::
    ( ToData datum
    , ToData redeemer
    , FromData datum
    , FromData redeemer
    , Show datum
    , Show redeemer
    ) =>
    (datum -> redeemer -> Value -> Example) ->
    Methodology datum ->
    Methodology redeemer ->
    Methodology Value ->
    Generator 'ForSpending
  -- | @since 4.1
  GenForMinting ::
    ( ToData redeemer
    , FromData redeemer
    , Show redeemer
    ) =>
    (redeemer -> Value -> Example) ->
    Methodology redeemer ->
    Methodology Value ->
    Generator 'ForMinting

{- | Generate using 'Arbitrary' instances. A 'Methodology' for 'Value' has to be
 passed manually, as it's (currently) missing an instance.

 @since 3.1
-}
fromArbitrarySpending ::
  forall (datum :: Type) (redeemer :: Type).
  ( ToData datum
  , FromData datum
  , Arbitrary datum
  , Show datum
  , ToData redeemer
  , FromData redeemer
  , Show redeemer
  , Arbitrary redeemer
  ) =>
  (datum -> redeemer -> Value -> Example) ->
  Methodology Value ->
  Generator 'ForSpending
fromArbitrarySpending f = GenForSpending f fromArbitrary fromArbitrary

{- | Generate using 'Arbitrary' instances.

 @since 4.1
-}
fromArbitraryMinting ::
  forall (redeemer :: Type).
  ( ToData redeemer
  , FromData redeemer
  , Show redeemer
  , Arbitrary redeemer
  ) =>
  (redeemer -> Value -> Example) ->
  Methodology Value ->
  Generator 'ForMinting
fromArbitraryMinting f = GenForMinting f fromArbitrary
