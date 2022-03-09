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
  ContextFragment (..),

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

  -- ** Naming
  named,
  deleteNamed,

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
  ContextBuilder (ContextBuilder),
  ContextFragment (
    ContextFragment,
    cfDatums,
    cfInputs,
    cfMinting,
    cfOutputs,
    cfSignatures,
    cfValidatorInputs,
    cfValidatorOutputs
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

{- | Name a 'ContextFragment', producing a 'ContextBuilder'.

 @since 2.0
-}
named ::
  forall (p :: Purpose).
  ContextFragment p ->
  Text ->
  ContextBuilder p
named cf name = ContextBuilder . Map.singleton name $ cf

{- | Remove a part of a 'ContextBuilder' by name, if it exists.

 @since 2.0
-}
deleteNamed ::
  forall (p :: Purpose).
  Text ->
  ContextBuilder p ->
  ContextBuilder p
deleteNamed name (ContextBuilder cfs) = ContextBuilder . Map.delete name $ cfs

{- | Single 'SideUTXO'-input context fragment.

 @since 2.0
-}
input ::
  forall (p :: Purpose).
  SideUTXO ->
  ContextFragment p
input x = mempty {cfInputs = pure x}

{- | Single 'ValidatorUTXO'-input context fragment.

 = Note

 This input won't be used as 'Spending' in the 'ScriptPurpose'
 of any 'ScriptContext' built from the resulting fragment.

 @since 2.0
-}
validatorInput ::
  forall (d :: Type) (r :: Type).
  (FromData d, ToData d, Show d) =>
  Text ->
  ValidatorUTXO d ->
  ContextFragment ( 'ForSpending d r)
validatorInput name x =
  mempty {cfValidatorInputs = ValidatorUTXOs $ Map.singleton name x}

{- | Single 'SideUTXO'-output context fragment.

 @since 2.0
-}
output ::
  forall (p :: Purpose).
  SideUTXO ->
  ContextFragment p
output x = mempty {cfOutputs = pure x}

{- | Single 'ValidatorUTXO'-output context fragment.

 @since 2.0
-}
validatorOutput ::
  forall (d :: Type) (r :: Type).
  (FromData d, ToData d, Show d) =>
  Text ->
  ValidatorUTXO d ->
  ContextFragment ( 'ForSpending d r)
validatorOutput name x =
  mempty {cfValidatorOutputs = ValidatorUTXOs $ Map.singleton name x}

{- | Context fragment with one signature.

 @since 2.0
-}
signedWith ::
  forall (p :: Purpose).
  PubKeyHash ->
  ContextFragment p
signedWith x = mempty {cfSignatures = pure x}

{- | Context fragment with one additional datum.

 @since 2.0
-}
datum ::
  forall (p :: Purpose).
  BuiltinData ->
  ContextFragment p
datum x = mempty {cfDatums = pure x}

{- | Short for @'datum' '.' 'toBuiltinData'@.

 @since 2.0
-}
addDatum ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  a ->
  ContextFragment p
addDatum = datum . toBuiltinData

{- | Context fragment with 'Minting' value using a minting policy other than
 the one being tested.

 = Note

 Do not use this for 'Value's being minted by the minting policy you want to test.

 Asset classes with 'CurrencySymbol's matching 'testCurrencySymbol' in
 'TransactionConfig' will be excluded from the resulting 'ScriptContext'.

 @since 2.0
-}
minting ::
  forall (p :: Purpose).
  Minting ->
  ContextFragment p
minting x = mempty {cfMinting = pure x}

{- | Context fragment with an output at the given public key with the given
 'Value'.

 @since 2.0
-}
outToPubKey ::
  forall (p :: Purpose).
  PubKeyHash ->
  Value ->
  ContextFragment p
outToPubKey pkh =
  output . SideUTXO (PubKeyUTXO pkh Nothing) . GeneralValue

{- | As 'outToPubKey', but with attached datum.

 @since 2.0
-}
outToPubKeyWithDatum ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  PubKeyHash ->
  Value ->
  a ->
  ContextFragment p
outToPubKeyWithDatum pkh val dt =
  let valueType = GeneralValue val
      utxoType = PubKeyUTXO pkh (Just $ toBuiltinData dt)
   in output $ SideUTXO utxoType valueType

{- | As 'outToPubKey', but using 'Tokens'. These 'Tokens' are controlled
 by the 'MintingPolicy' for which the context is built.

 @since 2.0
-}
outTokensToPubKey ::
  forall (r :: Type).
  PubKeyHash ->
  Tokens ->
  ContextFragment ( 'ForMinting r)
outTokensToPubKey pkh (Tokens tn pos) =
  let valueType = TokensValue tn pos
      utxoType = PubKeyUTXO pkh Nothing
   in output $ SideUTXO utxoType valueType

{- | As 'outTokensToPubKey', but with attached datum.

 @since 2.0
-}
outTokensToPubKeyWithDatum ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  PubKeyHash ->
  Tokens ->
  a ->
  ContextFragment p
outTokensToPubKeyWithDatum pkh (Tokens tn pos) dt =
  let valueType = TokensValue tn pos
      utxoType = PubKeyUTXO pkh (Just $ toBuiltinData dt)
   in output $ SideUTXO utxoType valueType

{- | As 'outToPubKey', but using Lovelace.

 @since 2.0
-}
outLovelaceToPubKey ::
  forall (p :: Purpose).
  PubKeyHash ->
  Integer ->
  ContextFragment p
outLovelaceToPubKey pkh = outToPubKey pkh . lovelaceValueOf

{- | As 'outLovelaceToPubKey', but with attached datum.

 @since 2.0
-}
outLovelaceToPubKeyWithDatum ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  PubKeyHash ->
  Integer ->
  a ->
  ContextFragment p
outLovelaceToPubKeyWithDatum pkh int dt =
  outToPubKeyWithDatum pkh (lovelaceValueOf int) (toBuiltinData dt)

{- | Context fragment with an output to the script that is associated
 with the 'ScriptContext' being built.

 @since 2.0
-}
outToValidator ::
  forall (d :: Type) (r :: Type).
  (FromData d, ToData d, Show d) =>
  Text ->
  Value ->
  d ->
  ContextFragment ( 'ForSpending d r)
outToValidator name v dt = validatorOutput name (ValidatorUTXO dt v)

{- | Сontext fragment with an output to a script that is /not/ associated
 with the 'ScriptContext' being built.

 @since 2.0
-}
outToOtherScript ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  ValidatorHash ->
  Value ->
  a ->
  ContextFragment p
outToOtherScript hash val dt =
  let valueType = GeneralValue val
      utxoType = ScriptUTXO hash (toBuiltinData dt)
   in output $ SideUTXO utxoType valueType

{- | Сontext fragment with an output to a script with the given 'Tokens' controlled
 by the 'MintingPolicy' for which the 'ScriptContext' is built.

 @since 2.0
-}
outTokensToOtherScript ::
  forall (a :: Type) (r :: Type).
  (ToData a) =>
  ValidatorHash ->
  Tokens ->
  a ->
  ContextFragment ( 'ForMinting r)
outTokensToOtherScript hash (Tokens tn pos) dt =
  let valueType = TokensValue tn pos
      utxoType = ScriptUTXO hash (toBuiltinData dt)
   in output $ SideUTXO utxoType valueType

{- | Context fragment with an input from given public key with the given 'Value'.

 @since 2.0
-}
inFromPubKey ::
  forall (p :: Purpose).
  PubKeyHash ->
  Value ->
  ContextFragment p
inFromPubKey pkh value =
  let valueType = GeneralValue value
      utxoType = PubKeyUTXO pkh Nothing
   in input $ SideUTXO utxoType valueType

{- | As 'inFromPubKey', but with the given datum.

 @since 2.0
-}
inFromPubKeyWithDatum ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  PubKeyHash ->
  Value ->
  a ->
  ContextFragment p
inFromPubKeyWithDatum pkh val dt =
  let valueType = GeneralValue val
      utxoType = PubKeyUTXO pkh (Just $ toBuiltinData dt)
   in input $ SideUTXO utxoType valueType

{- | As 'inFromPubKey', but using 'Tokens'. These 'Tokens' are controlled
 by the 'MintingPolicy' for which the context is built.

 @since 2.0
-}
inTokensFromPubKey ::
  forall (r :: Type).
  PubKeyHash ->
  Tokens ->
  ContextFragment ( 'ForMinting r)
inTokensFromPubKey pkh (Tokens tn pos) =
  let valueType = TokensValue tn pos
      utxoType = PubKeyUTXO pkh Nothing
   in input $ SideUTXO utxoType valueType

{- | As 'inFromPubKey', but with the given datum.

 @since 1.0
-}
inTokensFromPubKeyWithDatum ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  PubKeyHash ->
  Tokens ->
  a ->
  ContextFragment p
inTokensFromPubKeyWithDatum pkh (Tokens tn pos) dt =
  let valueType = TokensValue tn pos
      utxoType = PubKeyUTXO pkh (Just $ toBuiltinData dt)
   in input $ SideUTXO utxoType valueType

{- | As 'inFromPubKey', but using Lovelace.

 @since 2.0
-}
inLovelaceFromPubKey ::
  forall (p :: Purpose).
  PubKeyHash ->
  Integer ->
  ContextFragment p
inLovelaceFromPubKey pkh = inFromPubKey pkh . lovelaceValueOf

{- | As 'inLovelaceFromPubKey', but with attached datum.

 @since 2.0
-}
inLovelaceFromPubKeyWithDatum ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  PubKeyHash ->
  Integer ->
  a ->
  ContextFragment p
inLovelaceFromPubKeyWithDatum pkh int dt =
  inFromPubKeyWithDatum pkh (lovelaceValueOf int) (toBuiltinData dt)

{- | Context fragment with an input from the script that is associated
 with the 'ScriptContext' being built. This input won`t be used
 as 'Spending' in the 'ScriptPurpose' of the 'ScriptContext'.

 @since 2.0
-}
inFromValidator ::
  forall (d :: Type) (r :: Type).
  (FromData d, ToData d, Show d) =>
  Text ->
  Value ->
  d ->
  ContextFragment ( 'ForSpending d r)
inFromValidator name val dt = validatorInput name (ValidatorUTXO dt val)

{- | Сontext fragment with an input from a script that is not associated
 with the 'ScriptContext' being built.

 @since 2.0
-}
inFromOtherScript ::
  forall (p :: Purpose) (datum :: Type).
  (ToData datum) =>
  ValidatorHash ->
  Value ->
  datum ->
  ContextFragment p
inFromOtherScript hash val dt =
  let valueType = GeneralValue val
      utxoType = ScriptUTXO hash (toBuiltinData dt)
   in input $ SideUTXO utxoType valueType

{- | Сontext fragment with an input to a script with the given 'Tokens' controlled
 by the 'MintingPolicy' for which the 'ScriptContext' is built.

 @since 2.0
-}
inTokensFromOtherScript ::
  forall (datum :: Type) (r :: Type).
  (ToData datum) =>
  ValidatorHash ->
  Tokens ->
  datum ->
  ContextFragment ( 'ForMinting r)
inTokensFromOtherScript hash (Tokens tn pos) dt =
  let valueType = TokensValue tn pos
      utxoType = ScriptUTXO hash (toBuiltinData dt)
   in input $ SideUTXO utxoType valueType

{- | Context fragment with a minted 'Value'. The value is minted with a 'MintingPolicy'
 that is not associated with the 'ScriptContext' being built.

 @since 1.0
-}
mintedValue ::
  forall (p :: Purpose).
  Value ->
  ContextFragment p
mintedValue = minting . Mint

{- | Create 'PubKeyHash' from 'Wallet'.

 @since 1.0
-}
walletPubKeyHash :: Wallet -> PubKeyHash
walletPubKeyHash = unPaymentPubKeyHash . mockWalletPaymentPubKeyHash
