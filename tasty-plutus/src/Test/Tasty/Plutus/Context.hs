{-# LANGUAGE Trustworthy #-}

{- |
 Module: Test.Tasty.Plutus.Context
 Copyright: (C) MLabs 2021
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 An interface for building up Plutus script contexts for testing purposes.
-}
module Test.Tasty.Plutus.Context (
  -- * Types

  -- ** Classification and labelling
  Internal.Purpose (..),

  -- ** Building contexts
  Internal.ExternalType (..),
  Internal.Input (..),
  Internal.Output (..),
  Internal.Minting (..),
  Internal.ContextBuilder,

  -- * Functions

  -- ** Basic construction
  input,
  output,
  signedWith,
  datum,
  addDatum,
  minting,

  -- ** Paying
  paysToPubKey,
  paysToWallet,
  paysLovelaceToPubKey,
  paysLovelaceToWallet,
  paysSelf,
  paysOther,

  -- ** Spending
  spendsFromPubKey,
  spendsFromWallet,
  spendsFromPubKeySigned,
  spendsFromWalletSigned,
  spendsFromOther,

  -- ** Minting
  mintsWithSelf,
  mintsValue,
) where

import Data.Kind (Type)
import Data.Sequence qualified as Seq
import Ledger.Crypto (pubKeyHash)
import Plutus.V1.Ledger.Ada (lovelaceValueOf)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Scripts (ValidatorHash)
import Plutus.V1.Ledger.Value (TokenName, Value)
import PlutusTx.Builtins (BuiltinData)
import PlutusTx.IsData.Class (ToData (toBuiltinData))
import Test.Tasty.Plutus.Internal qualified as Internal
import Wallet.Emulator.Types (Wallet, walletPubKey)

{- | Single-input context.

 @since 1.0
-}
input ::
  forall (p :: Internal.Purpose).
  Internal.Input ->
  Internal.ContextBuilder p
input x =
  Internal.ContextBuilder (Seq.singleton x) mempty mempty mempty mempty

{- | Single-output context.

 @since 1.0
-}
output ::
  forall (p :: Internal.Purpose).
  Internal.Output ->
  Internal.ContextBuilder p
output x =
  Internal.ContextBuilder mempty (Seq.singleton x) mempty mempty mempty

{- | Context with one signature.

 @since 1.0
-}
signedWith ::
  forall (p :: Internal.Purpose).
  PubKeyHash ->
  Internal.ContextBuilder p
signedWith pkh =
  Internal.ContextBuilder mempty mempty (Seq.singleton pkh) mempty mempty

{- | Context with one additional datum.

 @since 1.0
-}
datum ::
  forall (p :: Internal.Purpose).
  BuiltinData ->
  Internal.ContextBuilder p
datum d =
  Internal.ContextBuilder mempty mempty mempty (Seq.singleton d) mempty

{- | Short for @'datum' '.' 'toBuiltinData'@.

 @since 3.0
-}
addDatum ::
  forall (p :: Internal.Purpose) (a :: Type).
  (ToData a) =>
  a ->
  Internal.ContextBuilder p
addDatum = datum . toBuiltinData

{- | Context with one minting.

 @since 3.0
-}
minting ::
  Internal.Minting ->
  Internal.ContextBuilder 'Internal.ForMinting
minting =
  Internal.ContextBuilder mempty mempty mempty mempty . Seq.singleton

{- | Indicate that a payment must happen to the given public key, worth the
 given amount.

 @since 1.0
-}
paysToPubKey ::
  forall (p :: Internal.Purpose).
  PubKeyHash ->
  Value ->
  Internal.ContextBuilder p
paysToPubKey pkh =
  output . Internal.Output (Internal.PubKeyType pkh)

{- | Indicate that a payment must happen to the given 'Wallet', worth the
 given amount.

 @since 1.0
-}
paysToWallet ::
  forall (p :: Internal.Purpose).
  Wallet ->
  Value ->
  Internal.ContextBuilder p
paysToWallet wallet = paysToPubKey (walletPubKeyHash wallet)

{- | Indicate that the script being tested must pay itself the given amount.

 @since 1.0
-}
paysSelf ::
  forall (p :: Internal.Purpose) (a :: Type).
  (ToData a) =>
  Value ->
  a ->
  Internal.ContextBuilder p
paysSelf v dt =
  output . Internal.Output (Internal.OwnType . toBuiltinData $ dt) $ v

{- | Indicate that the script being tested must pay another script the given
 amount.

 @since 1.0
-}
paysOther ::
  forall (p :: Internal.Purpose) (a :: Type).
  (ToData a) =>
  ValidatorHash ->
  Value ->
  a ->
  Internal.ContextBuilder p
paysOther hash v dt =
  output . Internal.Output (Internal.ScriptType hash . toBuiltinData $ dt) $ v

{- | As 'paysToPubKey', but using Lovelace.

 @since 3.0
-}
paysLovelaceToPubKey ::
  forall (p :: Internal.Purpose).
  PubKeyHash ->
  Integer ->
  Internal.ContextBuilder p
paysLovelaceToPubKey pkh = paysToPubKey pkh . lovelaceValueOf

{- | As 'paysToWallet', but using Lovelace.

 @since 3.0
-}
paysLovelaceToWallet ::
  forall (p :: Internal.Purpose).
  Wallet ->
  Integer ->
  Internal.ContextBuilder p
paysLovelaceToWallet wallet = paysToWallet wallet . lovelaceValueOf

{- | Indicate that the given amount must be spent from the given public key.

 @since 1.0
-}
spendsFromPubKey ::
  forall (p :: Internal.Purpose).
  PubKeyHash ->
  Value ->
  Internal.ContextBuilder p
spendsFromPubKey pkh =
  input . Internal.Input (Internal.PubKeyType pkh)

{- | As 'spendsFromPubKey', with an added signature.

 @since 1.0
-}
spendsFromPubKeySigned ::
  forall (p :: Internal.Purpose).
  PubKeyHash ->
  Value ->
  Internal.ContextBuilder p
spendsFromPubKeySigned pkh v = spendsFromPubKey pkh v <> signedWith pkh

{- | Indicate that the given amount must be spent from the given 'Wallet'.

 @since 1.0
-}
spendsFromWallet ::
  forall (p :: Internal.Purpose).
  Wallet ->
  Value ->
  Internal.ContextBuilder p
spendsFromWallet wallet = spendsFromPubKey (walletPubKeyHash wallet)

{- | As 'spendsFromWallet', with an added signature.

 @since 1.0
-}
spendsFromWalletSigned ::
  forall (p :: Internal.Purpose).
  Wallet ->
  Value ->
  Internal.ContextBuilder p
spendsFromWalletSigned wallet = spendsFromPubKeySigned (walletPubKeyHash wallet)

{- | Indicate that the given amount must be spent from another script.

 @since 1.0
-}
spendsFromOther ::
  forall (p :: Internal.Purpose) (datum :: Type).
  (ToData datum) =>
  ValidatorHash ->
  Value ->
  datum ->
  Internal.ContextBuilder p
spendsFromOther hash v d =
  input . Internal.Input (Internal.ScriptType hash . toBuiltinData $ d) $ v

{- | Indicate that an amount of a given token must be minted.

 @since 3.0
-}
mintsWithSelf ::
  TokenName ->
  Integer ->
  Internal.ContextBuilder 'Internal.ForMinting
mintsWithSelf tn = minting . Internal.OwnMint tn

{- | Indicate that someone must mint the given 'Value'.

 @since 3.0
-}
mintsValue ::
  Value ->
  Internal.ContextBuilder 'Internal.ForMinting
mintsValue = minting . Internal.OtherMint

-- Helpers

walletPubKeyHash :: Wallet -> PubKeyHash
walletPubKeyHash = pubKeyHash . walletPubKey
