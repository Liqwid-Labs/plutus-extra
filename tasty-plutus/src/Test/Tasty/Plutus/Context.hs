{-# LANGUAGE Trustworthy #-}

{- |
 Module: Test.Tasty.Plutus.Context
 Copyright: (C) MLabs 2021
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 An interface for building up Plutus script contexts for testing purposes.

 = Note on self-spending

 Currently, if you need to set up a context for spending from the script itself,
 you need to pass a `Value` to the 'Test.Tasty.Plutus.TestData.TestData' that
 is used to run the test. This is counter-intuitive, and will be fixed in a
 future release.
-}
module Test.Tasty.Plutus.Context (
  -- * Types

  -- ** Classification and labelling
  Purpose (..),

  -- ** Building contexts
  ExternalType (..),
  Input (..),
  Output (..),
  Minting (..),
  ContextBuilder,

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

  -- ** Combining Contexts
  makeIncompleteContexts,
) where

import Data.Kind (Type)
import Data.Sequence qualified as Seq
import Plutus.V1.Ledger.Ada (lovelaceValueOf)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Scripts (ValidatorHash)
import Plutus.V1.Ledger.Value (TokenName, Value)
import PlutusTx.Builtins (BuiltinData)
import PlutusTx.IsData.Class (ToData (toBuiltinData))
import Test.Tasty.Plutus.Internal.Context (
  ContextBuilder (ContextBuilder),
  ExternalType (OwnType, PubKeyType, ScriptType),
  Input (Input),
  Minting (OtherMint, OwnMint),
  Output (Output),
  Purpose (ForMinting, ForSpending),
  makeIncompleteContexts,
 )
import Wallet.Emulator.Types (Wallet, walletPubKeyHash)

{- | Single-input context.

 @since 1.0
-}
input ::
  forall (p :: Purpose).
  Input ->
  ContextBuilder p
input x =
  ContextBuilder (Seq.singleton x) mempty mempty mempty mempty

{- | Single-output context.

 @since 1.0
-}
output ::
  forall (p :: Purpose).
  Output ->
  ContextBuilder p
output x =
  ContextBuilder mempty (Seq.singleton x) mempty mempty mempty

{- | Context with one signature.

 @since 1.0
-}
signedWith ::
  forall (p :: Purpose).
  PubKeyHash ->
  ContextBuilder p
signedWith pkh =
  ContextBuilder mempty mempty (Seq.singleton pkh) mempty mempty

{- | Context with one additional datum.

 @since 1.0
-}
datum ::
  forall (p :: Purpose).
  BuiltinData ->
  ContextBuilder p
datum d =
  ContextBuilder mempty mempty mempty (Seq.singleton d) mempty

{- | Short for @'datum' '.' 'toBuiltinData'@.

 @since 3.0
-}
addDatum ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  a ->
  ContextBuilder p
addDatum = datum . toBuiltinData

{- | Context with one minting.

 @since 3.2
-}
minting ::
  forall (p :: Purpose).
  Minting ->
  ContextBuilder p
minting =
  ContextBuilder mempty mempty mempty mempty . Seq.singleton

{- | Indicate that a payment must happen to the given public key, worth the
 given amount.

 @since 1.0
-}
paysToPubKey ::
  forall (p :: Purpose).
  PubKeyHash ->
  Value ->
  ContextBuilder p
paysToPubKey pkh =
  output . Output (PubKeyType pkh)

{- | Indicate that a payment must happen to the given 'Wallet', worth the
 given amount.

 @since 1.0
-}
paysToWallet ::
  forall (p :: Purpose).
  Wallet ->
  Value ->
  ContextBuilder p
paysToWallet wallet = paysToPubKey (walletPubKeyHash wallet)

{- | Indicate that the script being tested must pay itself the given amount.

 @since 1.0
-}
paysSelf ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  Value ->
  a ->
  ContextBuilder p
paysSelf v dt =
  output . Output (OwnType . toBuiltinData $ dt) $ v

{- | Indicate that the script being tested must pay another script the given
 amount.

 @since 1.0
-}
paysOther ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  ValidatorHash ->
  Value ->
  a ->
  ContextBuilder p
paysOther hash v dt =
  output . Output (ScriptType hash . toBuiltinData $ dt) $ v

{- | As 'paysToPubKey', but using Lovelace.

 @since 3.0
-}
paysLovelaceToPubKey ::
  forall (p :: Purpose).
  PubKeyHash ->
  Integer ->
  ContextBuilder p
paysLovelaceToPubKey pkh = paysToPubKey pkh . lovelaceValueOf

{- | As 'paysToWallet', but using Lovelace.

 @since 3.0
-}
paysLovelaceToWallet ::
  forall (p :: Purpose).
  Wallet ->
  Integer ->
  ContextBuilder p
paysLovelaceToWallet wallet = paysToWallet wallet . lovelaceValueOf

{- | Indicate that the given amount must be spent from the given public key.

 @since 1.0
-}
spendsFromPubKey ::
  forall (p :: Purpose).
  PubKeyHash ->
  Value ->
  ContextBuilder p
spendsFromPubKey pkh =
  input . Input (PubKeyType pkh)

{- | As 'spendsFromPubKey', with an added signature.

 @since 1.0
-}
spendsFromPubKeySigned ::
  forall (p :: Purpose).
  PubKeyHash ->
  Value ->
  ContextBuilder p
spendsFromPubKeySigned pkh v = spendsFromPubKey pkh v <> signedWith pkh

{- | Indicate that the given amount must be spent from the given 'Wallet'.

 @since 1.0
-}
spendsFromWallet ::
  forall (p :: Purpose).
  Wallet ->
  Value ->
  ContextBuilder p
spendsFromWallet wallet = spendsFromPubKey (walletPubKeyHash wallet)

{- | As 'spendsFromWallet', with an added signature.

 @since 1.0
-}
spendsFromWalletSigned ::
  forall (p :: Purpose).
  Wallet ->
  Value ->
  ContextBuilder p
spendsFromWalletSigned wallet = spendsFromPubKeySigned (walletPubKeyHash wallet)

{- | Indicate that the given amount must be spent from another script.

 @since 1.0
-}
spendsFromOther ::
  forall (p :: Purpose) (datum :: Type).
  (ToData datum) =>
  ValidatorHash ->
  Value ->
  datum ->
  ContextBuilder p
spendsFromOther hash v d =
  input . Input (ScriptType hash . toBuiltinData $ d) $ v

{- | Indicate that an amount of a given token must be minted.

 @since 3.2
-}
mintsWithSelf ::
  forall (p :: Purpose).
  TokenName ->
  Integer ->
  ContextBuilder p
mintsWithSelf tn = minting . OwnMint tn

{- | Indicate that someone must mint the given 'Value'.

 @since 3.2
-}
mintsValue ::
  forall (p :: Purpose).
  Value ->
  ContextBuilder p
mintsValue = minting . OtherMint
