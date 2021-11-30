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
  fromArbitrarySpending,
  fromArbitraryMinting,
  Tokens (Tokens, unTokens),
  token,
) where

import Data.Kind (Type)
import Data.Semigroup (stimes, stimesIdempotent)
import Ledger.Typed.Scripts (DatumType, RedeemerType)
import Plutus.V1.Ledger.Value (Value)
import PlutusTx.IsData.Class (FromData, ToData)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink))
import Test.QuickCheck.Gen (Gen)
import Test.Tasty.Plutus.Internal.Context (
  Purpose (ForMinting, ForSpending),
  Tokens (Tokens, unTokens),
  token,
 )
import Prelude

{- | All the data needed to test a validator or minting policy.

 @since 3.0
-}
data TestData (s :: Type) (p :: Purpose) where
  -- | @since 3.0
  SpendingTest ::
    ( ToData (DatumType s)
    , ToData (RedeemerType s)
    , FromData (DatumType s)
    , FromData (RedeemerType s)
    , Show (DatumType s)
    , Show (RedeemerType s)
    ) =>
    -- | The input datum.
    --
    -- @since 3.0
    DatumType s ->
    -- | The input redeemer.
    --
    -- @since 3.0
    RedeemerType s ->
    -- | The value to be spent from the script.
    --
    -- @since 3.0
    Value ->
    TestData s 'ForSpending
  MintingTest ::
    ( ToData (RedeemerType s)
    , FromData (RedeemerType s)
    , Show (RedeemerType s)
    ) =>
    -- | The input redeemer.
    --
    -- @since 3.0
    RedeemerType s ->
    -- | The tokens to be minted by the script.
    --
    -- @since 4.1
    Tokens ->
    TestData s 'ForMinting

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

{- | Helper wrapper function

 @since 4.1
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

{- | Contains a means of generating a 'TestData', as well as a way to determine
 a \'good\' or \'bad\' generated case.

 @since 3.1
-}
data Generator (s :: Type) (p :: Purpose) where
  -- | @since 3.1
  GenForSpending ::
    ( ToData (DatumType s)
    , ToData (RedeemerType s)
    , FromData (DatumType s)
    , FromData (RedeemerType s)
    , Show (DatumType s)
    , Show (RedeemerType s)
    ) =>
    (DatumType s -> RedeemerType s -> Value -> Outcome) ->
    Methodology (DatumType s) ->
    Methodology (RedeemerType s) ->
    Methodology Value ->
    Generator s 'ForSpending
  -- | @since 4.1
  GenForMinting ::
    ( ToData (RedeemerType s)
    , FromData (RedeemerType s)
    , Show (RedeemerType s)
    ) =>
    (RedeemerType s -> Tokens -> Outcome) ->
    Methodology (RedeemerType s) ->
    Methodology Tokens ->
    Generator s 'ForMinting

{- | Generate using 'Arbitrary' instances. A 'Methodology' for 'Value' has to be
 passed manually, as it's (currently) missing an instance.

 @since 3.1
-}
fromArbitrarySpending ::
  forall (s :: Type).
  ( ToData (DatumType s)
  , FromData (DatumType s)
  , Arbitrary (DatumType s)
  , Show (DatumType s)
  , ToData (RedeemerType s)
  , FromData (RedeemerType s)
  , Show (RedeemerType s)
  , Arbitrary (RedeemerType s)
  ) =>
  (DatumType s -> RedeemerType s -> Value -> Outcome) ->
  Methodology Value ->
  Generator s 'ForSpending
fromArbitrarySpending f = GenForSpending f fromArbitrary fromArbitrary

{- | Generate using 'Arbitrary' instances.

 @since 4.1
-}
fromArbitraryMinting ::
  forall (s :: Type).
  ( ToData (RedeemerType s)
  , FromData (RedeemerType s)
  , Show (RedeemerType s)
  , Arbitrary (RedeemerType s)
  ) =>
  (RedeemerType s -> Tokens -> Outcome) ->
  Methodology Tokens ->
  Generator s 'ForMinting
fromArbitraryMinting f = GenForMinting f fromArbitrary
