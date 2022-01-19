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
  outputsToInputs,

  -- ** Basic construction
  input,
  output,
  signedWith,
  datum,
  addDatum,
  minting,

  -- ** Paying
  paysToPubKey,
  paysToPubKeyWithDatum,
  paysTokensToPubKey,
  paysToWallet,
  paysToWalletWithDatum,
  paysTokensToWallet,
  paysLovelaceToPubKey,
  paysLovelaceToWallet,
  paysToSelf,
  paysToOther,
  paysTokensToOther,

  -- ** Spending
  spendsFromPubKey,
  spendsFromPubKeyWithDatum,
  spendsTokensFromPubKey,
  spendsFromPubKeySigned,
  spendsFromPubKeyWithDatumSigned,
  spendsTokensFromPubKeySigned,
  spendsFromWallet,
  spendsFromWalletWithDatum,
  spendsTokensFromWallet,
  spendsFromWalletSigned,
  spendsFromWalletWithDatumSigned,
  spendsTokensFromWalletSigned,
  spendsFromOther,
  spendsTokensFromOther,

  -- ** Minting
  mintsValue,

  -- ** Combining Contexts
  makeIncompleteContexts,
) where

import Data.Kind (Type)
import Data.Sequence qualified as Seq
import Plutus.V1.Ledger.Ada (lovelaceValueOf)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Scripts (ValidatorHash)
import Plutus.V1.Ledger.Value (Value)
import PlutusTx.Builtins (BuiltinData)
import PlutusTx.IsData.Class (ToData (toBuiltinData))
import Test.Tasty.Plutus.Internal.Context (
  ContextBuilder (ContextBuilder),
  ExternalType (OwnType, PubKeyType, ScriptType),
  Input (Input),
  Minting (Mint),
  Output (Output),
  Purpose (ForMinting, ForSpending),
  ValueType (GeneralValue, TokensValue),
  makeIncompleteContexts,
  outputsToInputs,
 )
import Test.Tasty.Plutus.Internal.Minting (Tokens (Tokens))
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

{- | Context with 'Minting' value using a minting policy other than the tested one.

 = Note

 Do not use this for 'Value' being minted by the tested minting policy.

 Asset classes with 'CurrencySymbol' matching testCurrencySymbol in 'TransactionConfig'
 will be excluded from the resulting 'ScriptContext'.

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
  output . Output (PubKeyType pkh Nothing) . GeneralValue

{- | Indicate that a payment must happen to the given public key, worth the
 given amount and the given datum attached.

 @since 5.3
-}
paysToPubKeyWithDatum ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  PubKeyHash ->
  Value ->
  a ->
  ContextBuilder p
paysToPubKeyWithDatum pkh val dt =
  let value = GeneralValue val
      outType = PubKeyType pkh (Just $ toBuiltinData dt)
   in output $ Output outType value

{- | Indicate that the given 'Tokens' controlled by the tested minting policy
must be paid to the given public key.

 @since 6.0
-}
paysTokensToPubKey ::
  forall (r :: Type).
  PubKeyHash ->
  Tokens ->
  ContextBuilder ( 'ForMinting r)
paysTokensToPubKey pkh (Tokens tn pos) =
  output . Output (PubKeyType pkh Nothing) $ TokensValue tn pos

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

{- | Indicate that a payment must happen to the given 'Wallet', worth the
 given amount and the given datum attached.

 @since 5.3
-}
paysToWalletWithDatum ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  Wallet ->
  Value ->
  a ->
  ContextBuilder p
paysToWalletWithDatum wallet = paysToPubKeyWithDatum (walletPubKeyHash wallet)

{- | Indicate that the given 'Tokens' controlled by the tested minting policy
 must be paid to the given 'Wallet'.

 @since 6.0
-}
paysTokensToWallet ::
  forall (r :: Type).
  Wallet ->
  Tokens ->
  ContextBuilder ( 'ForMinting r)
paysTokensToWallet wallet = paysTokensToPubKey (walletPubKeyHash wallet)

{- | Indicate that a payment must happen to the script being tested, worth
 the given amount.

 @since 6.0
-}
paysToSelf ::
  forall (d :: Type) (r :: Type).
  (ToData d) =>
  Value ->
  d ->
  ContextBuilder ( 'ForSpending d r)
paysToSelf v dt =
  output . Output (OwnType . toBuiltinData $ dt) $ GeneralValue v

{- | Indicate that a payment must happen to another script, worth the
 given amount.

 @since 4.0
-}
paysToOther ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  ValidatorHash ->
  Value ->
  a ->
  ContextBuilder p
paysToOther hash v dt =
  output . Output (ScriptType hash . toBuiltinData $ dt) $ GeneralValue v

{- | Indicate that the given 'Tokens' controlled by the tested minting policy
 must be paid to another script.

 @since 6.0
-}
paysTokensToOther ::
  forall (a :: Type) (r :: Type).
  (ToData a) =>
  ValidatorHash ->
  Tokens ->
  a ->
  ContextBuilder ( 'ForMinting r)
paysTokensToOther hash (Tokens tn pos) dt =
  output . Output (ScriptType hash . toBuiltinData $ dt) $ TokensValue tn pos

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
  input . Input (PubKeyType pkh Nothing) . GeneralValue

{- | Indicate that the given amount must be spent from the given public key,
 with the given datum attached.

 @since 5.3
-}
spendsFromPubKeyWithDatum ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  PubKeyHash ->
  Value ->
  a ->
  ContextBuilder p
spendsFromPubKeyWithDatum pkh val dt =
  let value = GeneralValue val
      inType = PubKeyType pkh (Just $ toBuiltinData dt)
   in input $ Input inType value

{- | Indicate that the given 'Tokens' controlled by the tested minting policy
 must be spent from the given public key.

 @since 6.0
-}
spendsTokensFromPubKey ::
  forall (r :: Type).
  PubKeyHash ->
  Tokens ->
  ContextBuilder ( 'ForMinting r)
spendsTokensFromPubKey pkh (Tokens tn pos) =
  input . Input (PubKeyType pkh Nothing) $ TokensValue tn pos

{- | As 'spendsFromPubKey', with an added signature.

 @since 1.0
-}
spendsFromPubKeySigned ::
  forall (p :: Purpose).
  PubKeyHash ->
  Value ->
  ContextBuilder p
spendsFromPubKeySigned pkh v = signedWith pkh <> spendsFromPubKey pkh v

{- | As 'spendsTokensFromPubKey', with an added signature.

 @since 6.0
-}
spendsTokensFromPubKeySigned ::
  forall (r :: Type).
  PubKeyHash ->
  Tokens ->
  ContextBuilder ( 'ForMinting r)
spendsTokensFromPubKeySigned pkh (Tokens tn pos) =
  signedWith pkh <> spendsTokensFromPubKey pkh (Tokens tn pos)

{- | As 'spendsFromPubKeyWithDatum', with an added signature.

 @since 5.3
-}
spendsFromPubKeyWithDatumSigned ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  PubKeyHash ->
  Value ->
  a ->
  ContextBuilder p
spendsFromPubKeyWithDatumSigned pkh v dt = spendsFromPubKeyWithDatum pkh v dt <> signedWith pkh

{- | Indicate that the given amount must be spent from the given 'Wallet'.

 @since 1.0
-}
spendsFromWallet ::
  forall (p :: Purpose).
  Wallet ->
  Value ->
  ContextBuilder p
spendsFromWallet wallet = spendsFromPubKey (walletPubKeyHash wallet)

{- | Indicate that the given 'Tokens' controlled by the tested minting policy
 must be spent from the given 'Wallet'

 @since 6.0
-}
spendsTokensFromWallet ::
  forall (r :: Type).
  Wallet ->
  Tokens ->
  ContextBuilder ( 'ForMinting r)
spendsTokensFromWallet wallet = spendsTokensFromPubKey (walletPubKeyHash wallet)

{- | Indicate that the given amount must be spent from the given 'Wallet'.

 @since 5.3
-}
spendsFromWalletWithDatum ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  Wallet ->
  Value ->
  a ->
  ContextBuilder p
spendsFromWalletWithDatum wallet = spendsFromPubKeyWithDatum (walletPubKeyHash wallet)

{- | As 'spendsFromWallet', with an added signature.

 @since 1.0
-}
spendsFromWalletSigned ::
  forall (p :: Purpose).
  Wallet ->
  Value ->
  ContextBuilder p
spendsFromWalletSigned wallet =
  spendsFromPubKeySigned (walletPubKeyHash wallet)

{- | As 'spendsTokensFromWallet', with an added signature.

 @since 6.0
-}
spendsTokensFromWalletSigned ::
  forall (r :: Type).
  Wallet ->
  Tokens ->
  ContextBuilder ( 'ForMinting r)
spendsTokensFromWalletSigned wallet =
  spendsTokensFromPubKeySigned (walletPubKeyHash wallet)

{- | As 'spendsFromWalletWithDatum', with an added signature.

 @since 5.3
-}
spendsFromWalletWithDatumSigned ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  Wallet ->
  Value ->
  a ->
  ContextBuilder p
spendsFromWalletWithDatumSigned wallet = spendsFromPubKeyWithDatumSigned (walletPubKeyHash wallet)

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
  input . Input (ScriptType hash . toBuiltinData $ d) $ GeneralValue v

{- | Indicate that the given 'Tokens' controlled by the tested minting policy
 must be spent from another script.

 @since 6.0
-}
spendsTokensFromOther ::
  forall (datum :: Type) (r :: Type).
  (ToData datum) =>
  ValidatorHash ->
  Tokens ->
  datum ->
  ContextBuilder ( 'ForMinting r)
spendsTokensFromOther hash (Tokens tn pos) d =
  input . Input (ScriptType hash . toBuiltinData $ d) $ TokensValue tn pos

{- | Indicate that the given 'Value' must be minted with a minting policy
 other than the tested one.

 @since 3.2
-}
mintsValue ::
  forall (p :: Purpose).
  Value ->
  ContextBuilder p
mintsValue = minting . Mint
