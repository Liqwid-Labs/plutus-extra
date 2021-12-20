module Test.Tasty.Plutus.Internal.Minting (
  -- * Types
  MintingPolicyQuery (..),
  Tokens (..),

  -- * Helpers
  processMintingQuery,

  -- * Functions
  burningTokens,
  mintingTokens,
) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Plutus.V1.Ledger.Api (TokenName)
import PlutusTx.Positive (Positive, getPositive)
import PlutusTx.Prelude qualified as PTx
import Prelude

{- | Tokens to be minted or burned by minting policy.

  -- This type is 'Semigroup' but not 'Monoid', as a minting policy cannot be
  -- triggered if no tokens are minted.

  @since 4.1
-}
newtype Tokens = Tokens {unTokens :: NonEmpty MintingPolicyQuery}
  deriving stock
    ( -- | @since 4.1
      Eq
    , -- | @since 4.1
      Show
    )
  deriving newtype
    ( -- | @since 4.1
      Semigroup
    )

data MintingPolicyQuery
  = MintQuery TokenName Positive
  | BurnQuery TokenName Positive
  deriving stock
    ( -- | @since 6.0
      Eq
    , -- | @since 6.0
      Show
    )

processMintingQuery :: MintingPolicyQuery -> (TokenName, Integer)
processMintingQuery = \case
  MintQuery tn pos -> (tn,) . getPositive $ pos
  BurnQuery tn pos -> (tn,) . negate . getPositive $ i

{- | Create `MintingPolicyQuery` for minting tokens

  @since 6.0
-}
mintingTokens :: TokenName -> Positive -> NonEmpty MintingPolicyQuery
mintingTokens tn i = MintQuery tn i :| []

{- | Create `MintingPolicyQuery` for burning tokens

  @since 6.0
-}
burningTokens :: TokenName -> Positive -> NonEmpty MintingPolicyQuery
burningTokens tn i = BurnQuery tn i :| []
