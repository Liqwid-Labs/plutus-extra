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

  -- ** Errors
  DecodeFailure (..),

  -- ** Classification and labelling
  Purpose (..),

  -- ** Building contexts
  InputType (..),
  OutputType (..),
  Input (..),
  Output (..),
  Minting (..),
  ContextBuilder,

  -- ** Transaction configuration
  TransactionConfig (..),
  defaultTransactionConfig,

  -- * Functions

  -- ** Basic construction
  input,
  output,
  signedWith,
  datum,

  -- ** Paying
  paysToPubKey,
  paysToWallet,
  paysSelf,
  paysOther,

  -- ** Spending
  spendsFromPubKey,
  spendsFromWallet,
  spendsFromPubKeySigned,
  spendsFromWalletSigned,
  spendsFromSelf,
  spendsFromOther,

  -- ** Compilation
  compileSpending,
  compileMinting,

  -- ** Rendering
  renderDecodeFailure,
) where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Validation (Validation (Failure))
import GHC.Exts (toList)
import Ledger.Address (pubKeyHashAddress, scriptHashAddress)
import Ledger.Crypto (PubKeyHash, pubKeyHash)
import Ledger.Scripts (Datum (Datum), DatumHash, ValidatorHash, datumHash)
import Ledger.Value (CurrencySymbol, TokenName, Value (Value))
import Plutus.V1.Ledger.Contexts (
  ScriptContext (ScriptContext),
  ScriptPurpose (Spending),
  TxInInfo (TxInInfo),
  TxInfo (TxInfo),
  TxOut (TxOut),
  TxOutRef (TxOutRef),
 )
import Plutus.V1.Ledger.Interval (Interval)
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Time (POSIXTime)
import Plutus.V1.Ledger.TxId (TxId (TxId))
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins (BuiltinData)
import PlutusTx.IsData.Class (FromData (fromBuiltinData), ToData (toBuiltinData))
import Text.PrettyPrint (Doc, integer, nest, ($+$), (<+>))
import Text.Show.Pretty (ppDoc)
import Wallet.Emulator.Types (Wallet, walletPubKey)
import Witherable (iwither, mapMaybe)

{- | A representation of decode failures for datums and redeemers.

 @since 1.0
-}
data DecodeFailure
  = -- | @since 1.0
    BadDatumDecode Integer BuiltinData
  | -- | @since 1.0
    BadRedeemerDecode Integer BuiltinData
  deriving stock
    ( -- | @since 1.0
      Show
    )

{- | Renders a 'DecodeFailure'.

 @since 2.0
-}
renderDecodeFailure :: DecodeFailure -> Doc
renderDecodeFailure = \case
  BadDatumDecode ix dat ->
    "Datum" <+> integer ix
      $+$ (nest 4 . ppDoc $ dat)
  BadRedeemerDecode ix dat ->
    "Redeemer" <+> integer ix
      $+$ (nest 4 . ppDoc $ dat)

{- | Describes what kind of validator this is meant to test. Directly
 corresponds to 'ScriptPurpose'.

 @since 1.0
-}
data Purpose
  = -- | Corresponds to 'Plutus.V1.Ledger.Contexts.Minting'.
    --
    -- @since 1.0
    ForMinting
  | -- | Corresponds to 'Plutus.V1.Ledger.Contexts.Spending'.
    --
    -- @since 1.0
    ForSpending

{- | Different input types, and some of their metadata.

 @since 1.0
-}
data InputType
  = -- | @since 1.0
    PubKeyInput PubKeyHash
  | -- | @since 1.0
    ScriptInput ValidatorHash BuiltinData
  | -- | This is in the order of \'datum, redeemer\'.
    --
    --   @since 1.0
    OwnInput BuiltinData BuiltinData
  deriving stock
    ( -- | @since 1.0
      Show
    )

{- | Different output types, and some of their metadata.

 @since 1.0
-}
data OutputType
  = -- | @since 1.0
    PubKeyOutput PubKeyHash
  | -- | @since 1.0
    ScriptOutput ValidatorHash BuiltinData
  | -- | @since 1.0
    OwnOutput BuiltinData
  deriving stock
    ( -- | @since 1.0
      Show
    )

{- | An input to a script, consisting of a value and a type.

 @since 1.0
-}
data Input
  = -- | @since 1.0
    Input InputType Value
  deriving stock
    ( -- | @since 1.0
      Show
    )

{- | An output from a script, consisting of a value and a type.

 @since 1.0
-}
data Output
  = -- | @since 1.0
    Output OutputType Value
  deriving stock
    ( -- | @since 1.0
      Show
    )

{- | A minting result.

 @since 3.0
-}
data Minting
  = -- | @since 3.0
    OwnMint TokenName Integer
  | -- | @since 3.0
    OtherMint Value
  deriving stock
    ( -- | @since 3.0
      Show
    )

{- | Sets certain parameters around the transaction a validator is supposed to
 test.

 @since 3.0
-}
data TransactionConfig = TransactionConfig
  { -- | The fee paid.
    --
    -- @since 3.0
    testFee :: Value
  , -- | Valid time range.
    --
    -- @since 3.0
    testTimeRange :: Interval POSIXTime
  , -- | Consume the inputs of this 'TxId'.
    --
    -- @since 3.0
    testTxId :: TxId
  , -- | The script's 'CurrencySymbol'.
    --
    -- @since 3.0
    testCurrencySymbol :: CurrencySymbol
  , -- | The validator address.
    --
    -- @since 3.0
    testValidatorHash :: ValidatorHash
  }
  deriving stock
    ( -- | @since 3.0
      Show
    )

{- | A transaction configuration with the following settings:

 * 'testFee' is the empty 'Value'.
 * 'testTimeRange' is 'always'.
 * Other values are arbitrary

 In particular, only 'testFee' and 'testTimeRange' are assumed to be stable;
 if you want specific values, set them manually.

 @since 3.0
-}
defaultTransactionConfig :: TransactionConfig
defaultTransactionConfig =
  TransactionConfig
    { testFee = mempty
    , testTimeRange = Interval.always
    , testTxId = TxId "abcd"
    , testCurrencySymbol = "ff"
    , testValidatorHash = "90ab"
    }

{- | A way to incrementally build up a script context.

 It is tagged with a 'Purpose' as a marker for what kind of script it's
 supposed to be validating. For now, only 'Spending' is supported.

 You can use the 'Semigroup' instance of this type to build up larger
 contexts. For example:

 > let cb = 'paysToWallet' aWallet someValue <>
 >          'signedWith' aHash <>
 >          'tagged' aUniqueTag

 @since 1.0
-}
data ContextBuilder (p :: Purpose)
  = ContextBuilder
      (Seq Input)
      (Seq Output)
      (Seq PubKeyHash)
      (Seq BuiltinData)
      (Seq Minting)

-- | @since 1.0
deriving stock instance Show (ContextBuilder p)

-- | @since 1.0
instance Semigroup (ContextBuilder p) where
  {-# INLINEABLE (<>) #-}
  ContextBuilder is os pkhs ts ms <> ContextBuilder is' os' pkhs' ts' ms' =
    ContextBuilder (is <> is') (os <> os') (pkhs <> pkhs') (ts <> ts') (ms <> ms')

{- | Single-input context.

 @since 1.0
-}
input :: forall (p :: Purpose). Input -> ContextBuilder p
input x = ContextBuilder (Seq.singleton x) mempty mempty mempty mempty

{- | Single-output context.

 @since 1.0
-}
output :: forall (p :: Purpose). Output -> ContextBuilder p
output x = ContextBuilder mempty (Seq.singleton x) mempty mempty mempty

{- | Context with one signature.

 @since 1.0
-}
signedWith :: forall (p :: Purpose). PubKeyHash -> ContextBuilder p
signedWith pkh = ContextBuilder mempty mempty (Seq.singleton pkh) mempty mempty

{- | Context with one additional datum.

 @since 1.0
-}
datum :: forall (p :: Purpose). BuiltinData -> ContextBuilder p
datum d = ContextBuilder mempty mempty mempty (Seq.singleton d) mempty

-- TODO: addDatum, addTag

-- | @since 1.0
paysToPubKey ::
  forall (p :: Purpose).
  PubKeyHash ->
  Value ->
  ContextBuilder p
paysToPubKey pkh = output . Output (PubKeyOutput pkh)

-- | @since 1.0
paysToWallet ::
  forall (p :: Purpose).
  Wallet ->
  Value ->
  ContextBuilder p
paysToWallet wallet = paysToPubKey (walletPubKeyHash wallet)

-- | @since 1.0
paysSelf ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  Value ->
  a ->
  ContextBuilder p
paysSelf v dt = output . Output (OwnOutput . toBuiltinData $ dt) $ v

-- | @since 1.0
paysOther ::
  forall (p :: Purpose) (a :: Type).
  (ToData a) =>
  ValidatorHash ->
  Value ->
  a ->
  ContextBuilder p
paysOther hash v dt =
  output . Output (ScriptOutput hash . toBuiltinData $ dt) $ v

-- TODO: payLovelaceToPkh, payLovelaceToWallet

-- | @since 1.0
spendsFromPubKey ::
  forall (p :: Purpose).
  PubKeyHash ->
  Value ->
  ContextBuilder p
spendsFromPubKey pkh = input . Input (PubKeyInput pkh)

-- | @since 1.0
spendsFromPubKeySigned ::
  forall (p :: Purpose).
  PubKeyHash ->
  Value ->
  ContextBuilder p
spendsFromPubKeySigned pkh v = spendsFromPubKey pkh v <> signedWith pkh

-- | @since 1.0
spendsFromWallet ::
  forall (p :: Purpose).
  Wallet ->
  Value ->
  ContextBuilder p
spendsFromWallet wallet = spendsFromPubKey (walletPubKeyHash wallet)

-- | @since 1.0
spendsFromWalletSigned ::
  forall (p :: Purpose).
  Wallet ->
  Value ->
  ContextBuilder 'ForSpending
spendsFromWalletSigned wallet = spendsFromPubKeySigned (walletPubKeyHash wallet)

-- | @since 1.0
spendsFromSelf ::
  forall (p :: Purpose) (datum :: Type) (redeemer :: Type).
  (ToData datum, ToData redeemer) =>
  Value ->
  datum ->
  redeemer ->
  ContextBuilder p
spendsFromSelf v d r =
  input . Input (OwnInput (toBuiltinData d) . toBuiltinData $ r) $ v

-- | @since 1.0
spendsFromOther ::
  forall (p :: Purpose) (datum :: Type).
  (ToData datum) =>
  ValidatorHash ->
  Value ->
  datum ->
  ContextBuilder p
spendsFromOther hash v d =
  input . Input (ScriptInput hash . toBuiltinData $ d) $ v

-- TODO: mintWithThisScript, mintValue

-- TODO: compileSpending, compileMinting

{-
{- | Given a context builder, and expected datum and redeemer types, check if
 all inputs can decode into those types. If this fails, collect all failures
 and give up; otherwise, construct a 'Map' where positions of inputs (in the
 order they were provided to the builder) are mapped to script inputs.

 @since 1.0
-}
compile ::
  forall (datum :: Type) (redeemer :: Type) (p :: Purpose).
  (FromData datum, FromData redeemer) =>
  ContextBuilder p ->
  Validation [DecodeFailure] (Map Integer (datum, redeemer, ScriptContext))
compile (SpendingBuilder is os pkhs tags) =
  iwither go . Map.fromAscList $ indexedInputs
  where
    indexedInputs :: [(Integer, Input)]
    indexedInputs = zip [1 ..] . toList $ is
    go ::
      Integer ->
      Input ->
      Validation [DecodeFailure] (Maybe (datum, redeemer, ScriptContext))
    go ix (Input typ _) = case typ of
      OwnInput dat red -> case fromBuiltinData @datum dat of
        Nothing -> Failure [BadDatumDecode ix dat]
        Just dat' -> case fromBuiltinData @redeemer red of
          Nothing -> Failure [BadRedeemerDecode ix dat]
          Just red' -> pure $ do
            let ref = TxOutRef (TxId "testSpendingTxId") ix
            let purpose = Spending ref
            let info = mkTxInfo
            pure (dat', red', ScriptContext info purpose)
      _ -> pure Nothing
    mkTxInfo :: TxInfo
    mkTxInfo =
      TxInfo
        (uncurry createTxInInfo <$> indexedInputs)
        (toTxOut <$> toList os)
        mempty
        (Value AssocMap.empty)
        mempty
        mempty
        Interval.always
        (toList pkhs)
        mkInfoData
        (TxId "testSpendingTx")
    mkInfoData :: [(DatumHash, Datum)]
    mkInfoData =
      toList $
        mapMaybe toInputDatum is
          <> mapMaybe toOutputDatum os
          <> (datumWithHash <$> tags)
-}

-- Helpers

toInputDatum :: Input -> Maybe (DatumHash, Datum)
toInputDatum (Input typ _) = case typ of
  ScriptInput _ dt -> Just . datumWithHash $ dt
  OwnInput dt _ -> Just . datumWithHash $ dt
  PubKeyInput _ -> Nothing

toOutputDatum :: Output -> Maybe (DatumHash, Datum)
toOutputDatum (Output typ _) = case typ of
  ScriptOutput _ dt -> Just . datumWithHash $ dt
  OwnOutput dt -> Just . datumWithHash $ dt
  PubKeyOutput _ -> Nothing

datumWithHash :: BuiltinData -> (DatumHash, Datum)
datumWithHash dt = (datumHash dt', dt')
  where
    dt' :: Datum
    dt' = Datum dt

createTxInInfo :: Integer -> Input -> TxInInfo
createTxInInfo ix (Input typ v) =
  TxInInfo (TxOutRef (TxId "testTxId") ix) $ case typ of
    PubKeyInput pkh -> TxOut (pubKeyHashAddress pkh) v Nothing
    ScriptInput hash datum ->
      TxOut (scriptHashAddress hash) v . justDatumHash $ datum
    OwnInput datum _ ->
      TxOut (scriptHashAddress "") v . justDatumHash $ datum

justDatumHash :: BuiltinData -> Maybe DatumHash
justDatumHash = Just . datumHash . Datum

toTxOut :: Output -> TxOut
toTxOut (Output typ v) = case typ of
  PubKeyOutput pkh -> TxOut (pubKeyHashAddress pkh) v Nothing
  ScriptOutput hash datum ->
    TxOut (scriptHashAddress hash) v . justDatumHash $ datum
  OwnOutput datum ->
    TxOut (scriptHashAddress "") v . justDatumHash $ datum

walletPubKeyHash :: Wallet -> PubKeyHash
walletPubKeyHash = pubKeyHash . walletPubKey
