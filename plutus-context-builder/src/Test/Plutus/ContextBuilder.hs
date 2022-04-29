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
  SomeValidatedUTXO (..),
  TestUTXO (..),
  ValidatorUTXOs (..),
  Minting (..),
  Naming (..),
  ContextBuilder,
  ContextFragment (..),

  -- ** Minting
  MintingPolicyTask (..),
  MintingPolicyAction (..),
  Tokens (..),

  -- * Functions

  -- ** Basic construction
  input,
  validatorInput,
  validatedInput,
  output,
  validatorOutput,
  validatedOutput,
  signedWith,
  datum,
  addDatum,
  minting,

  -- ** Naming
  named,
  deleteNamed,
  alterNamed,
  unionNamed,

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

  -- ** Direct
  liftContextFragment,
  liftNamedContextFragment,
  foldBuilt,

  -- ** Build finished context
  spendingScriptContext,
  mintingScriptContext,
  spendingScriptContextDef,
  mintingScriptContextDef,

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
  ContextBuilder (NoNames, WithNames),
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
  Naming (Anonymous, Named),
  Purpose (ForMinting, ForSpending, ForTransaction),
  SideUTXO (SideUTXO, sUtxoType, sUtxoValue),
  SomeValidatedUTXO (SomeValidatedUTXO, someRedeemer, someSpendingScript, someUTxO),
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
  ValidatorUTXOs (MultiValidatorUTXOs, NoValidatorUTXOs, ValidatorUTXOs),
  ValueType (GeneralValue, TokensValue),
  defTransactionConfig,
  foldBuilt,
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

{- | Anonymous context from a single 'SideUTXO' input.

 @since 2.0
-}
input ::
  forall (p :: Purpose).
  SideUTXO ->
  ContextBuilder p 'Anonymous
input x = NoNames $ mempty {cfInputs = pure x}

{- | Anonymous context from a single 'ValidatorUTXO' input.

 = Note

 This input won't be used for spending in any 'ScriptPurpose' of any
 'ScriptContext' built from this.

 @since 2.0
-}
validatorInput ::
  forall (d :: Type) (r :: Type).
  (FromData d, ToData d, Show d) =>
  -- | Name of the input
  Text ->
  ValidatorUTXO d ->
  ContextBuilder ( 'ForSpending d r) 'Anonymous
validatorInput name x =
  NoNames $
    mempty
      { cfValidatorInputs =
          ValidatorUTXOs $ Map.singleton name x
      }

{- | Anonymous context from a single 'SomeValidatedUTXO' input.

 = Note

 This input won't be used for spending in any 'ScriptPurpose' of any
 'ScriptContext' built from this.

 @since 2.1
-}
validatedInput ::
  -- | Name of the input
  Text ->
  SomeValidatedUTXO ->
  ContextBuilder 'ForTransaction 'Anonymous
validatedInput name x =
  NoNames $
    mempty
      { cfValidatorInputs =
          MultiValidatorUTXOs $ Map.singleton name x
      }

{- | Anonymous context from a single 'SideUTXO' output.

 @since 2.0
-}
output ::
  forall (p :: Purpose).
  SideUTXO ->
  ContextBuilder p 'Anonymous
output x = NoNames $ mempty {cfOutputs = pure x}

{- | Anonymous context from a single 'ValidatorUTXO' output.

 @since 2.0
-}
validatorOutput ::
  forall (d :: Type) (r :: Type).
  (FromData d, ToData d, Show d) =>
  Text ->
  ValidatorUTXO d ->
  ContextBuilder ( 'ForSpending d r) 'Anonymous
validatorOutput name x =
  NoNames $
    mempty
      { cfValidatorOutputs =
          ValidatorUTXOs $ Map.singleton name x
      }

{- | Anonymous context from a single 'SomeValidatedUTXO' output.

 @since 2.1
-}
validatedOutput ::
  Text ->
  SomeValidatedUTXO ->
  ContextBuilder 'ForTransaction 'Anonymous
validatedOutput name x =
  NoNames $
    mempty
      { cfValidatorOutputs =
          MultiValidatorUTXOs $ Map.singleton name x
      }

{- | Anonymous context signed with one signature.

 @since 2.0
-}
signedWith ::
  forall (p :: Purpose).
  PubKeyHash ->
  ContextBuilder p 'Anonymous
signedWith x = NoNames $ mempty {cfSignatures = pure x}

{- | Anonymous context with one additional datum.

 @since 2.0
-}
datum ::
  forall (p :: Purpose).
  BuiltinData ->
  ContextBuilder p 'Anonymous
datum x = NoNames $ mempty {cfDatums = pure x}

{- | Short for @'datum' '.' 'toBuiltinData'@.

 @since 2.0
-}
addDatum ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  a ->
  ContextBuilder p 'Anonymous
addDatum = datum . toBuiltinData

{- | Anonymous context with a 'Minting' value, using a minting policy other than
 the one being tested.

 = Note

 Do not use this for 'Value's being minted by the minting policy you want to
 test with the 'ScriptContext' this would produce. Asset classes with
 'CurrencySymbol's matching 'testCurrencySymbol' in the 'TransactionConfig'
 provided when building the 'ScriptContext' will be excluded.

 @since 2.0
-}
minting ::
  forall (p :: Purpose).
  Minting ->
  ContextBuilder p 'Anonymous
minting x = NoNames $ mempty {cfMinting = pure x}

{- | Name an arbitrary sub-context. The argument order is designed for infix
 use:

 > subContext :: ContextBuilder p 'Named
 > subContext = (signedWith myPubKeyHash) `named` "usingMyPK"

 @since 2.0
-}
named ::
  forall (p :: Purpose).
  ContextBuilder p 'Anonymous ->
  Text ->
  ContextBuilder p 'Named
named (NoNames cf) name = WithNames . Map.singleton name $ cf

{- | Remove a sub-context by name. Does nothing if there is no sub-context by
 that name.

 @since 2.0
-}
deleteNamed ::
  forall (p :: Purpose).
  Text ->
  ContextBuilder p 'Named ->
  ContextBuilder p 'Named
deleteNamed name (WithNames cfs) = WithNames . Map.delete name $ cfs

{- | Update, add or modify a sub-context given a name. This is a very general
 operation, which can be used to implement almost any modification of a
 sub-context by name.

 @since 2.0
-}
alterNamed ::
  forall (p :: Purpose).
  -- | The input to this function will be 'Nothing' if no sub-context exists by
  -- the given name, and 'Just' if one does. If this function returns 'Nothing',
  -- any sub-context by the given name will be removed; if it returns 'Just', it
  -- will be set to whatever that is.
  (Maybe (ContextFragment p) -> Maybe (ContextFragment p)) ->
  Text ->
  ContextBuilder p 'Named ->
  ContextBuilder p 'Named
alterNamed f name (WithNames cfs) = WithNames . Map.alter f name $ cfs

{- | A more precise combination operation for named 'ContextBuilder's, allowing
 specifying what to do if both \'halves\' share named sub-contexts. This is
 unlike '<>', which always discards the first: see the documentation for the
 'Semigroup' instance for 'ContextBuilder' for details.

 @since 2.0
-}
unionNamed ::
  forall (p :: Purpose).
  -- | This function will be used to combine 'ContextFragment's labelled with
  -- the same name in both \'halves\' to produce the final result. The first
  -- argument is the 'ContextFragment' from the \'left\' 'ContextBuilder', the
  -- second is from the \'right\'.
  (ContextFragment p -> ContextFragment p -> ContextFragment p) ->
  ContextBuilder p 'Named ->
  ContextBuilder p 'Named ->
  ContextBuilder p 'Named
unionNamed f (WithNames cfs) (WithNames cfs') =
  WithNames . Map.unionWith f cfs $ cfs'

{- | As 'outToPubKey', but using 'Tokens'. These 'Tokens' are controlled by the
 'MintingPolicy' for which the context is built.

 @since 2.0
-}
outTokensToPubKey ::
  forall (r :: Type).
  PubKeyHash ->
  Tokens ->
  ContextBuilder ( 'ForMinting r) 'Anonymous
outTokensToPubKey pkh (Tokens tn pos) =
  let valueType = TokensValue tn pos
      utxoType = PubKeyUTXO pkh Nothing
   in output $ SideUTXO utxoType valueType

{- | Anonymous context with an output at the given public key, with the given
 'Value'.

 @since 2.0
-}
outToPubKey ::
  forall (p :: Purpose).
  PubKeyHash ->
  Value ->
  ContextBuilder p 'Anonymous
outToPubKey pkh =
  output . SideUTXO (PubKeyUTXO pkh Nothing) . GeneralValue

{- | As 'outToPubKey', but with an attached datum.

 @since 2.0
-}
outToPubKeyWithDatum ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  PubKeyHash ->
  Value ->
  a ->
  ContextBuilder p 'Anonymous
outToPubKeyWithDatum pkh val dt =
  let valueType = GeneralValue val
      utxoType = PubKeyUTXO pkh (Just $ toBuiltinData dt)
   in output $ SideUTXO utxoType valueType

{- | As 'outTokensToPubKey', but with an attached datum.

 @since 2.0
-}
outTokensToPubKeyWithDatum ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  PubKeyHash ->
  Tokens ->
  a ->
  ContextBuilder p 'Anonymous
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
  ContextBuilder p 'Anonymous
outLovelaceToPubKey pkh = outToPubKey pkh . lovelaceValueOf

{- | As 'outLovelaceToPubKey', but with an attached datum.

 @since 2.0
-}
outLovelaceToPubKeyWithDatum ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  PubKeyHash ->
  Integer ->
  a ->
  ContextBuilder p 'Anonymous
outLovelaceToPubKeyWithDatum pkh int dt =
  outToPubKeyWithDatum pkh (lovelaceValueOf int) (toBuiltinData dt)

{- | Anonymous context with an output to the script that is associated with the
 'ScriptContext' being built.

 @since 2.0
-}
outToValidator ::
  forall (d :: Type) (r :: Type).
  (FromData d, ToData d, Show d) =>
  Text ->
  Value ->
  d ->
  ContextBuilder ( 'ForSpending d r) 'Anonymous
outToValidator name v dt = validatorOutput name (ValidatorUTXO dt v)

{- | Anonymous context with an output to a script that is /not/ associated with
 the 'ScriptContext' being built.

 @since 2.0
-}
outToOtherScript ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  ValidatorHash ->
  Value ->
  a ->
  ContextBuilder p 'Anonymous
outToOtherScript hash val dt =
  let valueType = GeneralValue val
      utxoType = ScriptUTXO hash (toBuiltinData dt)
   in output $ SideUTXO utxoType valueType

{- | Anonymous context with an output to a script with the given 'Tokens',
 controlled by the 'MintingPolicy' for which the 'ScriptContext' is built.

 @since 2.0
-}
outTokensToOtherScript ::
  forall (a :: Type) (r :: Type).
  (ToData a) =>
  ValidatorHash ->
  Tokens ->
  a ->
  ContextBuilder ( 'ForMinting r) 'Anonymous
outTokensToOtherScript hash (Tokens tn pos) dt =
  let valueType = TokensValue tn pos
      utxoType = ScriptUTXO hash (toBuiltinData dt)
   in output $ SideUTXO utxoType valueType

{- | Anonymous context with an input from the given public key, with the given
 'Value'.

 @since 2.0
-}
inFromPubKey ::
  forall (p :: Purpose).
  PubKeyHash ->
  Value ->
  ContextBuilder p 'Anonymous
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
  ContextBuilder p 'Anonymous
inFromPubKeyWithDatum pkh val dt =
  let valueType = GeneralValue val
      utxoType = PubKeyUTXO pkh (Just $ toBuiltinData dt)
   in input $ SideUTXO utxoType valueType

{- | As 'inFromPubKey', but using 'Tokens'. These 'Tokens' are controlled by the
 'MintingPolicy' for which the context is built.

 @since 2.0
-}
inTokensFromPubKey ::
  forall (r :: Type).
  PubKeyHash ->
  Tokens ->
  ContextBuilder ( 'ForMinting r) 'Anonymous
inTokensFromPubKey pkh (Tokens tn pos) =
  let valueType = TokensValue tn pos
      utxoType = PubKeyUTXO pkh Nothing
   in input $ SideUTXO utxoType valueType

{- | As 'inTokensFromPubKey', but with the given datum.

 @since 2.0
-}
inTokensFromPubKeyWithDatum ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  PubKeyHash ->
  Tokens ->
  a ->
  ContextBuilder p 'Anonymous
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
  ContextBuilder p 'Anonymous
inLovelaceFromPubKey pkh = inFromPubKey pkh . lovelaceValueOf

{- | As 'inLovelaceFromPubKey', but with an attached datum.

 @since 2.0
-}
inLovelaceFromPubKeyWithDatum ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  PubKeyHash ->
  Integer ->
  a ->
  ContextBuilder p 'Anonymous
inLovelaceFromPubKeyWithDatum pkh int dt =
  inFromPubKeyWithDatum pkh (lovelaceValueOf int) (toBuiltinData dt)

{- | Anonymous context with an input from the script associated with the
 'ScriptContext' being built. This input won't be used in the spending part of
 the 'ScriptPurpose' in the resulting 'ScriptContext'.

 @since 2.0
-}
inFromValidator ::
  forall (d :: Type) (r :: Type).
  (FromData d, ToData d, Show d) =>
  Text ->
  Value ->
  d ->
  ContextBuilder ( 'ForSpending d r) 'Anonymous
inFromValidator name val dt = validatorInput name (ValidatorUTXO dt val)

{- | Anonymous context with an input from a script /not/ associated with the
 'ScriptContext' being built.

 @since 2.0
-}
inFromOtherScript ::
  forall (p :: Purpose) (datum :: Type).
  (ToData datum) =>
  ValidatorHash ->
  Value ->
  datum ->
  ContextBuilder p 'Anonymous
inFromOtherScript hash val dt =
  let valueType = GeneralValue val
      utxoType = ScriptUTXO hash (toBuiltinData dt)
   in input $ SideUTXO utxoType valueType

{- | Anonymous context with an input to a script with the given 'Tokens',
 controlled by the 'MintingPolicy' for which the 'ScriptContext' is being
 built.

 @since 2.0
-}
inTokensFromOtherScript ::
  forall (datum :: Type) (r :: Type).
  (ToData datum) =>
  ValidatorHash ->
  Tokens ->
  datum ->
  ContextBuilder ( 'ForMinting r) 'Anonymous
inTokensFromOtherScript hash (Tokens tn pos) dt =
  let valueType = TokensValue tn pos
      utxoType = ScriptUTXO hash (toBuiltinData dt)
   in input $ SideUTXO utxoType valueType

{- | Anonymous context with a minted 'Value'. This 'Value' is minted with a
 'MintingPolicy' that is /not/ associated with the 'ScriptContext' being
 built.

 @since 2.0
-}
mintedValue ::
  forall (p :: Purpose).
  Value ->
  ContextBuilder p 'Anonymous
mintedValue = minting . Mint

{- | Turns an arbitrary 'ContextFragment' into an anonymous 'ContextBuilder'.

 = Note

 This is a low-level operation designed for maximum control. If possible, use
 the other, higher-level, operations in this module instead.

 @since 2.0
-}
liftContextFragment ::
  forall (p :: Purpose).
  ContextFragment p ->
  ContextBuilder p 'Anonymous
liftContextFragment = NoNames

{- | Turns an arbitrary 'ContextFragment' into a named 'ContextBuilder',
 labelling with the given name.

 = Note

 This is a low-level operation designed for maximum control. If possible, use
 the other, higher-level, operations in this module instead.

 @since 2.0
-}
liftNamedContextFragment ::
  forall (p :: Purpose).
  Text ->
  ContextFragment p ->
  ContextBuilder p 'Named
liftNamedContextFragment name cf = NoNames cf `named` name

{- | Create a 'PubKeyHash' from a 'Wallet'.

 @since 1.0
-}
walletPubKeyHash :: Wallet -> PubKeyHash
walletPubKeyHash = unPaymentPubKeyHash . mockWalletPaymentPubKeyHash
