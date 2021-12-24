module Test.Tasty.Plutus.Internal.Minting (
  -- * Types
  MintingPolicyTask (..),
  MintingPolicyAction (..),
  Tokens (..),

  -- * Helpers
  burnTokens,
  mintTokens,
) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Plutus.V1.Ledger.Api (TokenName)
import PlutusTx.Positive (Positive)
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  CoArbitrary (coarbitrary),
  Function (function),
  functionMap,
 )
import Test.QuickCheck.Plutus.Instances ()
import Prelude

{- | The task for the tested minting policy.

 @since 6.0
-}
data MintingPolicyTask
  = MPTask MintingPolicyAction Tokens
  deriving stock
    ( -- | @since 6.0
      Eq
    , -- | @since 6.0
      Show
    )

{- | 'MintingPolicyAction' defines a list of actions for the minting policy.

 @since 6.0
-}
data MintingPolicyAction
  = BurnAction
  | MintAction
  deriving stock
    ( -- | @since 6.0
      Eq
    , -- | @since 6.0
      Show
    )

{- | 'Tokens' corresponds to some positive number of 'TokenName' belonging
 to the tested minting policy.

 @since 6.0
-}
data Tokens = Tokens TokenName Positive
  deriving stock
    ( -- | @since 6.0
      Eq
    , -- | @since 6.0
      Show
    )

-- | @since 6.0
instance Arbitrary Tokens where
  arbitrary = Tokens <$> arbitrary <*> arbitrary
  shrink (Tokens tn pos) = uncurry Tokens <$> shrink (tn, pos)

-- | @since 6.0
instance CoArbitrary Tokens where
  coarbitrary (Tokens tn pos) = coarbitrary (tn, pos)

-- | @since 6.0
instance Function Tokens where
  function = functionMap into from
    where
      into :: Tokens -> (TokenName, Positive)
      into (Tokens tn pos) = (tn, pos)
      from :: (TokenName, Positive) -> Tokens
      from = uncurry Tokens

{- | Create single `MPTask` for minting tokens

  @since 6.0
-}
mintTokens :: Tokens -> NonEmpty MintingPolicyTask
mintTokens toks = MPTask MintAction toks :| []

{- | Create single `MPTask` for burning tokens

  @since 6.0
-}
burnTokens :: Tokens -> NonEmpty MintingPolicyTask
burnTokens toks = MPTask BurnAction toks :| []
