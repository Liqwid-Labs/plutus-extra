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
  Outcome (..),
  passIf,

  -- * QuickCheck support
  Methodology (..),
  fromArbitrary,
  static,
  Generator (..),
  TestItems (..),
  Tokens (Tokens, unTokens),
  token,
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
  Tokens (Tokens, unTokens),
  token,
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
    -- | The input redeemer.
    --
    -- @since 3.0
    redeemer ->
    -- | The tokens to be minted by the script.
    --
    -- @since 4.1
    Tokens ->
    TestData 'ForMinting

{- | Describes whether a case, comprised of a script and a test data for the
 script, should pass or fail. Used to classify generated outputs for
 QuickCheck-based tests; also internally used to classify unit tests.

 @since 5.0
-}
data Outcome = Fail | Pass
  deriving stock
    ( -- | @since 5.0
      Eq
    , -- | @since 5.0
      Show
    )

{- | \'Maximal badness\': gives 'Fail' when any argument is 'Fail'.

 @since 4.0
-}
instance Semigroup Outcome where
  {-# INLINEABLE (<>) #-}
  Fail <> _ = Fail
  _ <> Fail = Fail
  _ <> _ = Pass
  {-# INLINEABLE stimes #-}
  stimes = stimesIdempotent

{- | Helper wrapper function for creating 'Outcome' from a condition.

 @since 5.0
-}
passIf :: Bool -> Outcome
passIf True = Pass
passIf False = Fail

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
 for creating 'TestItems' from the seed.

 @since 5.0
-}
data Generator (a :: Type) (p :: Purpose) where
  -- | @since 5.0
  GenForSpending ::
    (Show a) =>
    -- | 'Methodology' for seed
    -- @since 5.0
    Methodology a ->
    -- | Function for producing 'TestItems' from the seed
    -- @since 5.0
    (a -> TestItems 'ForSpending) ->
    Generator a 'ForSpending
  GenForMinting ::
    (Show a) =>
    -- | 'Methodology' for seed
    -- @since 5.0
    Methodology a ->
    -- | Function for producing 'TestItems' from the seed
    -- @since 5.0
    (a -> TestItems 'ForMinting) ->
    Generator a 'ForMinting

{- | Сontains the necessary data set for script checking.
 This dataset does not cover any set of tests or conditions.
 It is used only to check the result of calling the script with certain values.

 @since 5.0
-}
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
    { -- | Datum provided to the Validator
      -- @since 5.0
      spendDatum :: datum
    , -- | Redeemer provided to the Validator
      -- @since 5.0
      spendRedeemer :: redeemer
    , -- | Value spended from the Validator
      -- @since 5.0
      spendValue :: Value
    , -- | ContextBuilder used for creating ScriptContext
      -- @since 5.0
      spendCB :: ContextBuilder 'ForSpending
    , -- | Result expected from calling the Validator
      -- | @since 5.0
      spendOutcome :: Outcome
    } ->
    TestItems 'ForSpending
  -- | @since 5.0
  ItemsForMinting ::
    forall (redeemer :: Type).
    ( ToData redeemer
    , FromData redeemer
    , Show redeemer
    ) =>
    { -- | Redeemer provided to the MintingPolicy
      -- @since 5.0
      mintRedeemer :: redeemer
    , -- | Tokens minted with the MintingPolicy
      -- @since 5.0
      mintTokens :: Tokens
    , -- | ContextBuilder used for creating ScriptContext
      -- @since 5.0
      mintCB :: ContextBuilder 'ForMinting
    , -- | Result expected from calling the MintingPolicy
      -- | @since 5.0
      mintOutcome :: Outcome
    } ->
    TestItems 'ForMinting
