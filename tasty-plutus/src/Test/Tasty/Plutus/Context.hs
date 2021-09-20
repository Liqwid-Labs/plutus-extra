{- |
 Module: Test.Tasty.Plutus.Context
 Copyright: (C) MLabs 2021
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 An interface for building up Plutus validator contexts for testing purposes.
-}
module Test.Tasty.Plutus.Context (
  -- * Types

  -- ** Classification and labelling
  Internal.Purpose (..),

  -- ** Building contexts
  Internal.InputType (..),
  Internal.OutputType (..),
  Internal.Input (..),
  Internal.Output (..),
  Internal.Minting (..),
  Internal.ContextBuilder,

  -- ** Transaction configuration
  Internal.TransactionConfig (..),
  Internal.defaultTransactionConfig,

  -- * Functions

  -- ** Basic construction
  Internal.input,
  Internal.output,
  Internal.signedWith,
  Internal.datum,
  Internal.addDatum,
  Internal.minting,

  -- ** Paying
  Internal.paysToPubKey,
  Internal.paysToWallet,
  Internal.paysLovelaceToPubKey,
  Internal.paysLovelaceToWallet,
  Internal.paysSelf,
  Internal.paysOther,

  -- ** Spending
  Internal.spendsFromPubKey,
  Internal.spendsFromWallet,
  Internal.spendsFromPubKeySigned,
  Internal.spendsFromWalletSigned,
  Internal.spendsFromOther,

  -- ** Minting
  Internal.mintsWithSelf,
  Internal.mintsValue,
) where

import Test.Tasty.Plutus.Context.Internal qualified as Internal
