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
  defaultTransactionConfig,

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
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Scripts (ValidatorHash)
import Plutus.V1.Ledger.TxId (TxId (TxId))
import Plutus.V1.Ledger.Value (TokenName, Value)
import PlutusTx.Builtins (BuiltinData)
import PlutusTx.IsData.Class (ToData (toBuiltinData))
import Test.Tasty.Plutus.Context.Internal qualified as Internal
import Wallet.Emulator.Types (Wallet, walletPubKey)

{- | A transaction configuration with the following settings:

 * 'testFee' is the empty 'Value'.
 * 'testTimeRange' is 'always'.
 * Other values are arbitrary

 In particular, only 'testFee' and 'testTimeRange' are assumed to be stable;
 if you want specific values, set them manually.

 @since 3.0
-}
defaultTransactionConfig :: Internal.TransactionConfig
defaultTransactionConfig =
  Internal.TransactionConfig
    { Internal.testFee = mempty
    , Internal.testTimeRange = Interval.always
    , Internal.testTxId = TxId "abcd"
    , Internal.testCurrencySymbol = "ff"
    , Internal.testValidatorHash = "90ab"
    }

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
  forall (p :: Internal.Purpose).
  Internal.Minting ->
  Internal.ContextBuilder p
minting =
  Internal.ContextBuilder mempty mempty mempty mempty . Seq.singleton

-- | @since 1.0
paysToPubKey ::
  forall (p :: Internal.Purpose).
  PubKeyHash ->
  Value ->
  Internal.ContextBuilder p
paysToPubKey pkh =
  output . Internal.Output (Internal.PubKeyOutput pkh)

-- | @since 1.0
paysToWallet ::
  forall (p :: Internal.Purpose).
  Wallet ->
  Value ->
  Internal.ContextBuilder p
paysToWallet wallet = paysToPubKey (walletPubKeyHash wallet)

-- | @since 1.0
paysSelf ::
  forall (p :: Internal.Purpose) (a :: Type).
  (ToData a) =>
  Value ->
  a ->
  Internal.ContextBuilder p
paysSelf v dt =
  output . Internal.Output (Internal.OwnOutput . toBuiltinData $ dt) $ v

-- | @since 1.0
paysOther ::
  forall (p :: Internal.Purpose) (a :: Type).
  (ToData a) =>
  ValidatorHash ->
  Value ->
  a ->
  Internal.ContextBuilder p
paysOther hash v dt =
  output . Internal.Output (Internal.ScriptOutput hash . toBuiltinData $ dt) $ v

-- | @since 3.0
paysLovelaceToPubKey ::
  forall (p :: Internal.Purpose).
  PubKeyHash ->
  Integer ->
  Internal.ContextBuilder p
paysLovelaceToPubKey pkh = paysToPubKey pkh . lovelaceValueOf

-- | @since 3.0
paysLovelaceToWallet ::
  forall (p :: Internal.Purpose).
  Wallet ->
  Integer ->
  Internal.ContextBuilder p
paysLovelaceToWallet wallet = paysToWallet wallet . lovelaceValueOf

-- | @since 1.0
spendsFromPubKey ::
  forall (p :: Internal.Purpose).
  PubKeyHash ->
  Value ->
  Internal.ContextBuilder p
spendsFromPubKey pkh =
  input . Internal.Input (Internal.PubKeyInput pkh)

-- | @since 1.0
spendsFromPubKeySigned ::
  forall (p :: Internal.Purpose).
  PubKeyHash ->
  Value ->
  Internal.ContextBuilder p
spendsFromPubKeySigned pkh v = spendsFromPubKey pkh v <> signedWith pkh

-- | @since 1.0
spendsFromWallet ::
  forall (p :: Internal.Purpose).
  Wallet ->
  Value ->
  Internal.ContextBuilder p
spendsFromWallet wallet = spendsFromPubKey (walletPubKeyHash wallet)

-- | @since 1.0
spendsFromWalletSigned ::
  forall (p :: Internal.Purpose).
  Wallet ->
  Value ->
  Internal.ContextBuilder p
spendsFromWalletSigned wallet = spendsFromPubKeySigned (walletPubKeyHash wallet)

-- | @since 1.0
spendsFromOther ::
  forall (p :: Internal.Purpose) (datum :: Type).
  (ToData datum) =>
  ValidatorHash ->
  Value ->
  datum ->
  Internal.ContextBuilder p
spendsFromOther hash v d =
  input . Internal.Input (Internal.ScriptInput hash . toBuiltinData $ d) $ v

-- | @since 3.0
mintsWithSelf ::
  forall (p :: Internal.Purpose).
  TokenName ->
  Integer ->
  Internal.ContextBuilder p
mintsWithSelf tn = minting . Internal.OwnMint tn

-- | @since 3.0
mintsValue ::
  forall (p :: Internal.Purpose).
  Value ->
  Internal.ContextBuilder p
mintsValue = minting . Internal.OtherMint

-- Helpers

walletPubKeyHash :: Wallet -> PubKeyHash
walletPubKeyHash = pubKeyHash . walletPubKey
