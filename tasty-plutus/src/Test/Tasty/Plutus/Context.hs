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
  DecodeFailure (..),
  Purpose (..),
  InputType (..),
  OutputType (..),
  Input (..),
  Output (..),
  ContextBuilder,

  -- * Functions

  -- ** Basic construction
  input,
  output,
  signedWith,
  tagged,

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
  compile,

  -- ** Rendering
  renderDecodeFailure,
) where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Portray.Plutus (portrayBuiltinData)
import Data.Portray.Pretty (portrayalToDoc)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Validation (Validation (Failure))
import GHC.Exts (toList)
import Ledger.Address (pubKeyHashAddress, scriptHashAddress)
import Ledger.Crypto (PubKeyHash, pubKeyHash)
import Ledger.Scripts (Datum (Datum), DatumHash, ValidatorHash, datumHash)
import Ledger.Value (Value (Value))
import Plutus.V1.Ledger.Contexts (
  ScriptContext (ScriptContext),
  ScriptPurpose (Spending),
  TxInInfo (TxInInfo),
  TxInfo (TxInfo),
  TxOut (TxOut),
  TxOutRef (TxOutRef),
 )
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.TxId (TxId (TxId))
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins (BuiltinData)
import PlutusTx.IsData.Class (FromData (fromBuiltinData), ToData (toBuiltinData))
import Text.PrettyPrint (Doc, integer, nest, ($+$), (<+>))
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
      $+$ (nest 4 . portrayalToDoc . portrayBuiltinData $ dat)
  BadRedeemerDecode ix dat ->
    "Redeemer" <+> integer ix
      $+$ (nest 4 . portrayalToDoc . portrayBuiltinData $ dat)

{-
instance Pretty DecodeFailure where
  pretty = \case
    BadDatumDecode ix dat ->
      "Datum" <+> pretty ix <> hardline <> hang 4 (viaShow dat)
    BadRedeemerDecode ix dat ->
      "Redeemer" <+> pretty ix <> hardline <> hang 4 (viaShow dat)
-}

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
  | -- | Corresponds to 'Plutus.V1.Ledger.Contexts.Rewarding'.
    --
    -- @since 1.0
    ForRewarding
  | -- | Corresponds to 'Plutus.V1.Ledger.Contexts.Certifying'.
    --
    -- @since 1.0
    ForCertifying

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
data ContextBuilder (p :: Purpose) where
  SpendingBuilder ::
    Seq Input ->
    Seq Output ->
    Seq PubKeyHash ->
    Seq BuiltinData ->
    ContextBuilder 'ForSpending

-- | @since 1.0
deriving stock instance Show (ContextBuilder p)

-- | @since 1.0
instance Semigroup (ContextBuilder p) where
  {-# INLINEABLE (<>) #-}
  SpendingBuilder is os pkhs ts <> SpendingBuilder is' os' pkhs' ts' =
    SpendingBuilder (is <> is') (os <> os') (pkhs <> pkhs') (ts <> ts')

{- | Single-input context.

 @since 1.0
-}
input :: Input -> ContextBuilder 'ForSpending
input x = SpendingBuilder (Seq.singleton x) mempty mempty mempty

{- | Single-output context.

 @since 1.0
-}
output :: Output -> ContextBuilder 'ForSpending
output x = SpendingBuilder mempty (Seq.singleton x) mempty mempty

{- | Context with one signature.

 @since 1.0
-}
signedWith :: PubKeyHash -> ContextBuilder 'ForSpending
signedWith pkh = SpendingBuilder mempty mempty (Seq.singleton pkh) mempty

{- | Context with one tag.

 @since 1.0
-}
tagged :: BuiltinData -> ContextBuilder 'ForSpending
tagged = SpendingBuilder mempty mempty mempty . Seq.singleton

-- | @since 1.0
paysToPubKey :: PubKeyHash -> Value -> ContextBuilder 'ForSpending
paysToPubKey pkh = output . Output (PubKeyOutput pkh)

-- | @since 1.0
paysToWallet :: Wallet -> Value -> ContextBuilder 'ForSpending
paysToWallet wallet = paysToPubKey (walletPubKeyHash wallet)

-- | @since 1.0
paysSelf ::
  forall (a :: Type).
  (ToData a) =>
  Value ->
  a ->
  ContextBuilder 'ForSpending
paysSelf v dt = output . Output (OwnOutput . toBuiltinData $ dt) $ v

-- | @since 1.0
paysOther ::
  forall (a :: Type).
  (ToData a) =>
  ValidatorHash ->
  Value ->
  a ->
  ContextBuilder 'ForSpending
paysOther hash v dt =
  output . Output (ScriptOutput hash . toBuiltinData $ dt) $ v

-- | @since 1.0
spendsFromPubKey ::
  PubKeyHash ->
  Value ->
  ContextBuilder 'ForSpending
spendsFromPubKey pkh = input . Input (PubKeyInput pkh)

-- | @since 1.0
spendsFromPubKeySigned ::
  PubKeyHash ->
  Value ->
  ContextBuilder 'ForSpending
spendsFromPubKeySigned pkh v = spendsFromPubKey pkh v <> signedWith pkh

-- | @since 1.0
spendsFromWallet ::
  Wallet ->
  Value ->
  ContextBuilder 'ForSpending
spendsFromWallet wallet = spendsFromPubKey (walletPubKeyHash wallet)

-- | @since 1.0
spendsFromWalletSigned ::
  Wallet ->
  Value ->
  ContextBuilder 'ForSpending
spendsFromWalletSigned wallet = spendsFromPubKeySigned (walletPubKeyHash wallet)

-- | @since 1.0
spendsFromSelf ::
  forall (datum :: Type) (redeemer :: Type).
  (ToData datum, ToData redeemer) =>
  Value ->
  datum ->
  redeemer ->
  ContextBuilder 'ForSpending
spendsFromSelf v d r =
  input . Input (OwnInput (toBuiltinData d) . toBuiltinData $ r) $ v

-- | @since 1.0
spendsFromOther ::
  forall (datum :: Type).
  (ToData datum) =>
  ValidatorHash ->
  Value ->
  datum ->
  ContextBuilder 'ForSpending
spendsFromOther hash v d =
  input . Input (ScriptInput hash . toBuiltinData $ d) $ v

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
