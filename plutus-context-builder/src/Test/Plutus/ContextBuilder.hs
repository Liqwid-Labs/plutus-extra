{-# LANGUAGE Trustworthy #-}

{- |
 Module: Test.Plutus.ContextBuilder
 Copyright: (C) MLabs 2022
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 An interface for building up Plutus 'ScriptContext' for testing purposes.
-}
module Test.Plutus.ContextBuilder (
  -- * Types

  -- ** TransactionConfig
  TransactionConfig (..),
  InputPosition (..),

  -- ** Classification and labelling
  Purpose (..),

  -- ** Building contexts
  UTXOType (..),
  ValueType (..),
  SideUTXO (..),
  ValidatorUTXO (..),
  TestUTXO (..),
  ValidatorUTXOs (..),
  Minting (..),
  ContextBuilder (..),

  -- ** Minting
  MintingPolicyTask (..),
  MintingPolicyAction (..),
  Tokens (..),

  -- * Functions

  -- ** Build context
  spendingScriptContext,
  mintingScriptContext,
  spendingScriptContextDef,
  mintingScriptContextDef,

  -- ** Basic construction
  input,
  validatorInput,
  output,
  validatorOutput,
  signedWith,
  datum,
  addDatum,
  minting,

  -- ** Output
  outToPubKey,
  outToPubKeyWithDatum,
  outTokensToPubKey,
  outTokensToPubKeyWithDatum,
  outLovelaceToPubKey,
  outLovelaceToPubKeyWithDatum,
  outToValidator,
  outToOtherScript,
  outTokensToOtherScript,

  -- ** Spending
  inFromPubKey,
  inFromPubKeyWithDatum,
  inTokensFromPubKey,
  inTokensFromPubKeyWithDatum,
  inLovelaceFromPubKey,
  inLovelaceFromPubKeyWithDatum,
  inFromValidator,
  inFromOtherScript,
  inTokensFromOtherScript,

  -- ** Minting
  mintedValue,
  burnTokens,
  mintTokens,

  -- ** Utilities
  defTransactionConfig,
  makeIncompleteContexts,
  walletPubKeyHash,
) where

import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Ledger.Address (PaymentPubKeyHash (unPaymentPubKeyHash))
import Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Ada (lovelaceValueOf)
import Plutus.V1.Ledger.Scripts (ValidatorHash)
import Plutus.V1.Ledger.Value (Value)
import PlutusTx.Builtins (BuiltinData)
import PlutusTx.IsData.Class (FromData, ToData (toBuiltinData))
import Test.Plutus.ContextBuilder.Internal (
  ContextBuilder (
    ContextBuilder,
    cbDatums,
    cbInputs,
    cbMinting,
    cbOutputs,
    cbSignatures,
    cbValidatorInputs,
    cbValidatorOutputs
  ),
  InputPosition (Head, Tail),
  Minting (Mint),
  Purpose (ForMinting, ForSpending),
  SideUTXO (SideUTXO, sUtxoType, sUtxoValue),
  TestUTXO (TestUTXO, tUtxoDatum, tUtxoValue),
  TransactionConfig (
    TransactionConfig,
    testCurrencySymbol,
    testFee,
    testInputPosition,
    testTimeRange,
    testTxId,
    testValidatorHash
  ),
  UTXOType (PubKeyUTXO, ScriptUTXO),
  ValidatorUTXO (ValidatorUTXO, vUtxoDatum, vUtxoValue),
  ValidatorUTXOs (NoValidatorUTXOs, ValidatorUTXOs),
  ValueType (GeneralValue, TokensValue),
  defTransactionConfig,
  makeIncompleteContexts,
  mintingScriptContext,
  mintingScriptContextDef,
  spendingScriptContext,
  spendingScriptContextDef,
 )
import Test.Plutus.ContextBuilder.Minting (
  MintingPolicyAction (BurnAction, MintAction),
  MintingPolicyTask (MPTask),
  Tokens (Tokens),
  burnTokens,
  mintTokens,
 )
import Wallet.Emulator.Types (Wallet, mockWalletPaymentPubKeyHash)

{- | Single 'SideUTXO'-input context.

 @since 1.0
-}
input ::
  forall (p :: Purpose).
  Text ->
  SideUTXO ->
  ContextBuilder p
input name x = mempty {cbInputs = Map.singleton name x}

{- | Single 'ValidatorUTXO'-input context.

 = Note

 This input won't be used as 'Spending' in the 'ScriptPurpose'
 of the builded 'ScriptContext'.

 @since 1.0
-}
validatorInput ::
  forall (d :: Type) (r :: Type).
  (FromData d, ToData d, Show d) =>
  Text ->
  ValidatorUTXO d ->
  ContextBuilder ( 'ForSpending d r)
validatorInput name x =
  mempty {cbValidatorInputs = ValidatorUTXOs $ Map.singleton name x}

{- | Single 'SideUTXO'-output context.

 @since 1.0
-}
output ::
  forall (p :: Purpose).
  Text ->
  SideUTXO ->
  ContextBuilder p
output name x = mempty {cbOutputs = Map.singleton name x}

{- | Single 'ValidatorUTXO'-output context.

 @since 1.0
-}
validatorOutput ::
  forall (d :: Type) (r :: Type).
  (FromData d, ToData d, Show d) =>
  Text ->
  ValidatorUTXO d ->
  ContextBuilder ( 'ForSpending d r)
validatorOutput name x =
  mempty {cbValidatorOutputs = ValidatorUTXOs $ Map.singleton name x}

{- | Context with one signature.

 @since 1.0
-}
signedWith ::
  forall (p :: Purpose).
  Text ->
  PubKeyHash ->
  ContextBuilder p
signedWith name x = mempty {cbSignatures = Map.singleton name x}

{- | Context with one additional datum.

 @since 1.0
-}
datum ::
  forall (p :: Purpose).
  Text ->
  BuiltinData ->
  ContextBuilder p
datum name x = mempty {cbDatums = Map.singleton name x}

{- | Short for @'datum' '.' 'toBuiltinData'@.

 @since 1.0
-}
addDatum ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  Text ->
  a ->
  ContextBuilder p
addDatum name = datum name . toBuiltinData

{- | Context with 'Minting' value using a minting policy other than the tested one.

 = Note

 Do not use this for 'Value' being minted by the tested minting policy.

 Asset classes with 'CurrencySymbol' matching testCurrencySymbol in 'TransactionConfig'
 will be excluded from the resulting 'ScriptContext'.

 @since 1.0
-}
minting ::
  forall (p :: Purpose).
  Text ->
  Minting ->
  ContextBuilder p
minting name x = mempty {cbMinting = Map.singleton name x}

{- | Context with an output at the given public key with the given 'Value'.

 @since 1.0
-}
outToPubKey ::
  forall (p :: Purpose).
  Text ->
  PubKeyHash ->
  Value ->
  ContextBuilder p
outToPubKey name pkh =
  output name . SideUTXO (PubKeyUTXO pkh Nothing) . GeneralValue

{- | As 'outToPubKey', but with attached datum.

 @since 1.0
-}
outToPubKeyWithDatum ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  Text ->
  PubKeyHash ->
  Value ->
  a ->
  ContextBuilder p
outToPubKeyWithDatum name pkh val dt =
  let valueType = GeneralValue val
      utxoType = PubKeyUTXO pkh (Just $ toBuiltinData dt)
   in output name $ SideUTXO utxoType valueType

{- | As 'outToPubKey', but using 'Tokens'. These 'Tokens' are controlled
 by the 'MintingPolicy' for which the context is built.

 @since 1.0
-}
outTokensToPubKey ::
  forall (r :: Type).
  Text ->
  PubKeyHash ->
  Tokens ->
  ContextBuilder ( 'ForMinting r)
outTokensToPubKey name pkh (Tokens tn pos) =
  let valueType = TokensValue tn pos
      utxoType = PubKeyUTXO pkh Nothing
   in output name $ SideUTXO utxoType valueType

{- | As 'outTokensToPubKey', but with attached datum.

 @since 1.0
-}
outTokensToPubKeyWithDatum ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  Text ->
  PubKeyHash ->
  Tokens ->
  a ->
  ContextBuilder p
outTokensToPubKeyWithDatum name pkh (Tokens tn pos) dt =
  let valueType = TokensValue tn pos
      utxoType = PubKeyUTXO pkh (Just $ toBuiltinData dt)
   in output name $ SideUTXO utxoType valueType

{- | As 'outToPubKey', but using Lovelace.

 @since 1.0
-}
outLovelaceToPubKey ::
  forall (p :: Purpose).
  Text ->
  PubKeyHash ->
  Integer ->
  ContextBuilder p
outLovelaceToPubKey name pkh = outToPubKey name pkh . lovelaceValueOf

{- | As 'outLovelaceToPubKey', but with attached datum.

 @since 1.0
-}
outLovelaceToPubKeyWithDatum ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  Text ->
  PubKeyHash ->
  Integer ->
  a ->
  ContextBuilder p
outLovelaceToPubKeyWithDatum name pkh int dt =
  outToPubKeyWithDatum name pkh (lovelaceValueOf int) (toBuiltinData dt)

{- | Context with an output to the script that is associated
 with the 'ScriptContext' being built.

 @since 1.0
-}
outToValidator ::
  forall (d :: Type) (r :: Type).
  (FromData d, ToData d, Show d) =>
  Text ->
  Value ->
  d ->
  ContextBuilder ( 'ForSpending d r)
outToValidator name v dt = validatorOutput name (ValidatorUTXO dt v)

{- | Сontext with an output to a script that is not associated
 with the 'ScriptContext' being built.

 @since 1.0
-}
outToOtherScript ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  Text ->
  ValidatorHash ->
  Value ->
  a ->
  ContextBuilder p
outToOtherScript name hash val dt =
  let valueType = GeneralValue val
      utxoType = ScriptUTXO hash (toBuiltinData dt)
   in output name $ SideUTXO utxoType valueType

{- | Сontext with an output to a script with the given 'Tokens' controlled
 by the 'MintingPolicy' for which the 'ScriptContext' is built.

 @since 1.0
-}
outTokensToOtherScript ::
  forall (a :: Type) (r :: Type).
  (ToData a) =>
  Text ->
  ValidatorHash ->
  Tokens ->
  a ->
  ContextBuilder ( 'ForMinting r)
outTokensToOtherScript name hash (Tokens tn pos) dt =
  let valueType = TokensValue tn pos
      utxoType = ScriptUTXO hash (toBuiltinData dt)
   in output name $ SideUTXO utxoType valueType

{- | Context with an input from given public key with the given 'Value'.

 @since 1.0
-}
inFromPubKey ::
  forall (p :: Purpose).
  Text ->
  PubKeyHash ->
  Value ->
  ContextBuilder p
inFromPubKey name pkh value =
  let valueType = GeneralValue value
      utxoType = PubKeyUTXO pkh Nothing
   in input name $ SideUTXO utxoType valueType

{- | As 'inFromPubKey', but with the given datum.

 @since 1.0
-}
inFromPubKeyWithDatum ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  Text ->
  PubKeyHash ->
  Value ->
  a ->
  ContextBuilder p
inFromPubKeyWithDatum name pkh val dt =
  let valueType = GeneralValue val
      utxoType = PubKeyUTXO pkh (Just $ toBuiltinData dt)
   in input name $ SideUTXO utxoType valueType

{- | As 'inFromPubKey', but using 'Tokens'. These 'Tokens' are controlled
 by the 'MintingPolicy' for which the context is built.

 @since 1.0
-}
inTokensFromPubKey ::
  forall (r :: Type).
  Text ->
  PubKeyHash ->
  Tokens ->
  ContextBuilder ( 'ForMinting r)
inTokensFromPubKey name pkh (Tokens tn pos) =
  let valueType = TokensValue tn pos
      utxoType = PubKeyUTXO pkh Nothing
   in input name $ SideUTXO utxoType valueType

{- | As 'inFromPubKey', but with the given datum.

 @since 1.0
-}
inTokensFromPubKeyWithDatum ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  Text ->
  PubKeyHash ->
  Tokens ->
  a ->
  ContextBuilder p
inTokensFromPubKeyWithDatum name pkh (Tokens tn pos) dt =
  let valueType = TokensValue tn pos
      utxoType = PubKeyUTXO pkh (Just $ toBuiltinData dt)
   in input name $ SideUTXO utxoType valueType

{- | As 'inFromPubKey', but using Lovelace.

 @since 1.0
-}
inLovelaceFromPubKey ::
  forall (p :: Purpose).
  Text ->
  PubKeyHash ->
  Integer ->
  ContextBuilder p
inLovelaceFromPubKey name pkh = inFromPubKey name pkh . lovelaceValueOf

{- | As 'inLovelaceFromPubKey', but with attached datum.

 @since 1.0
-}
inLovelaceFromPubKeyWithDatum ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  Text ->
  PubKeyHash ->
  Integer ->
  a ->
  ContextBuilder p
inLovelaceFromPubKeyWithDatum name pkh int dt =
  inFromPubKeyWithDatum name pkh (lovelaceValueOf int) (toBuiltinData dt)

{- | Context with an input from the script that is associated
 with the 'ScriptContext' being built. This input won`t be used
 as 'Spending' in the 'ScriptPurpose' of the 'ScriptContext'.

 @since 1.0
-}
inFromValidator ::
  forall (d :: Type) (r :: Type).
  (FromData d, ToData d, Show d) =>
  Text ->
  Value ->
  d ->
  ContextBuilder ( 'ForSpending d r)
inFromValidator name val dt = validatorInput name (ValidatorUTXO dt val)

{- | Сontext with an input from a script that is not associated
 with the 'ScriptContext' being built.

 @since 1.0
-}
inFromOtherScript ::
  forall (p :: Purpose) (datum :: Type).
  (ToData datum) =>
  Text ->
  ValidatorHash ->
  Value ->
  datum ->
  ContextBuilder p
inFromOtherScript name hash val dt =
  let valueType = GeneralValue val
      utxoType = ScriptUTXO hash (toBuiltinData dt)
   in input name $ SideUTXO utxoType valueType

{- | Сontext with an input to a script with the given 'Tokens' controlled
 by the 'MintingPolicy' for which the 'ScriptContext' is built.

 @since 1.0
-}
inTokensFromOtherScript ::
  forall (datum :: Type) (r :: Type).
  (ToData datum) =>
  Text ->
  ValidatorHash ->
  Tokens ->
  datum ->
  ContextBuilder ( 'ForMinting r)
inTokensFromOtherScript name hash (Tokens tn pos) dt =
  let valueType = TokensValue tn pos
      utxoType = ScriptUTXO hash (toBuiltinData dt)
   in input name $ SideUTXO utxoType valueType

{- | Context with a minted 'Value'. The value is minted with a 'MintingPolicy'
 that is not associated with the 'ScriptContext' being built.

 @since 1.0
-}
mintedValue ::
  forall (p :: Purpose).
  Text ->
  Value ->
  ContextBuilder p
mintedValue name = minting name . Mint

{- | Create 'PubKeyHash' from 'Wallet'.

 @since 1.0
-}
walletPubKeyHash :: Wallet -> PubKeyHash
walletPubKeyHash = unPaymentPubKeyHash . mockWalletPaymentPubKeyHash
