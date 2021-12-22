module Test.Tasty.Plutus.Internal.Minting (
  -- * Types
  MintingPolicyQuery (..),

  -- * Helpers
  processMintingQuery,

  -- * Functions
  burningTokens,
  mintingTokens,
) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Plutus.V1.Ledger.Api (TokenName)
import PlutusTx.Positive (Positive, getPositive)
import Prelude

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
  BurnQuery tn pos -> (tn,) . negate . getPositive $ pos

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
