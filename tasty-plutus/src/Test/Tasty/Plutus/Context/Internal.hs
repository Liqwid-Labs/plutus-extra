-- Internals of context building.
module Test.Tasty.Plutus.Context.Internal (
  Purpose (..),
  InputType (..),
  OutputType (..),
  Input (..),
  Output (..),
  Minting (..),
  ContextBuilder (..),
  TransactionConfig (..),
  defaultTransactionConfig,
  input,
  output,
  signedWith,
  datum,
  addDatum,
  minting,
  paysToPubKey,
  paysToWallet,
  paysLovelaceToPubKey,
  paysLovelaceToWallet,
  paysSelf,
  paysOther,
  spendsFromPubKey,
  spendsFromWallet,
  spendsFromPubKeySigned,
  spendsFromWalletSigned,
  spendsFromOther,
  mintsWithSelf,
  mintsValue,
  compileSpending,
  compileMinting,
) where

import Data.Kind (Type)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import GHC.Exts (toList)
import Ledger.Ada (lovelaceValueOf)
import Ledger.Address (pubKeyHashAddress, scriptHashAddress)
import Ledger.Crypto (PubKeyHash, pubKeyHash)
import Ledger.Scripts (Datum (Datum), DatumHash, ValidatorHash, datumHash)
import Ledger.Value (CurrencySymbol, TokenName, Value)
import Plutus.V1.Ledger.Contexts (
  ScriptContext (ScriptContext),
  ScriptPurpose (Minting, Spending),
  TxInInfo (TxInInfo),
  TxInfo (
    TxInfo,
    txInfoDCert,
    txInfoData,
    txInfoFee,
    txInfoId,
    txInfoInputs,
    txInfoMint,
    txInfoOutputs,
    txInfoSignatories,
    txInfoValidRange,
    txInfoWdrl
  ),
  TxOut (TxOut),
  TxOutRef (TxOutRef),
 )
import Plutus.V1.Ledger.Interval (Interval)
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Time (POSIXTime)
import Plutus.V1.Ledger.TxId (TxId (TxId))
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx.Builtins (BuiltinData)
import PlutusTx.IsData.Class (ToData (toBuiltinData))
import Wallet.Emulator.Types (Wallet, walletPubKey)
import Witherable (mapMaybe)

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
  | -- | @since 3.0
    OwnInput BuiltinData
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
 supposed to be validating.

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

 @since 3.0
-}
minting ::
  forall (p :: Purpose).
  Minting ->
  ContextBuilder p
minting = ContextBuilder mempty mempty mempty mempty . Seq.singleton

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

-- | @since 3.0
paysLovelaceToPubKey ::
  forall (p :: Purpose).
  PubKeyHash ->
  Integer ->
  ContextBuilder p
paysLovelaceToPubKey pkh = paysToPubKey pkh . lovelaceValueOf

-- | @since 3.0
paysLovelaceToWallet ::
  forall (p :: Purpose).
  Wallet ->
  Integer ->
  ContextBuilder p
paysLovelaceToWallet wallet = paysToWallet wallet . lovelaceValueOf

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
  ContextBuilder p
spendsFromWalletSigned wallet = spendsFromPubKeySigned (walletPubKeyHash wallet)

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

-- | @since 3.0
mintsWithSelf ::
  forall (p :: Purpose).
  TokenName ->
  Integer ->
  ContextBuilder p
mintsWithSelf tn = minting . OwnMint tn

-- | @since 3.0
mintsValue ::
  forall (p :: Purpose).
  Value ->
  ContextBuilder p
mintsValue = minting . OtherMint

-- Helpers

compileSpending ::
  forall (datum :: Type).
  (ToData datum) =>
  TransactionConfig ->
  ContextBuilder 'ForSpending ->
  datum ->
  Value ->
  ScriptContext
compileSpending conf cb d val =
  ScriptContext go
    . Spending
    . TxOutRef (testTxId conf)
    $ 0
  where
    go :: TxInfo
    go =
      let dt = toBuiltinData d
          baseInfo = baseTxInfo conf cb
          inInfo = createTxInInfo 0 . Input (OwnInput dt) $ val
          inData = datumWithHash dt
       in baseInfo
            { txInfoInputs = inInfo : txInfoInputs baseInfo
            , txInfoData = inData : txInfoData baseInfo
            }

compileMinting ::
  TransactionConfig ->
  ContextBuilder 'ForMinting ->
  ScriptContext
compileMinting conf cb =
  ScriptContext go
    . Minting
    . testCurrencySymbol
    $ conf
  where
    go :: TxInfo
    go = baseTxInfo conf cb

baseTxInfo ::
  forall (p :: Purpose).
  TransactionConfig ->
  ContextBuilder p ->
  TxInfo
baseTxInfo conf (ContextBuilder ins outs pkhs dats mints) =
  TxInfo
    { txInfoInputs = uncurry createTxInInfo <$> indexedInputs
    , txInfoOutputs = toList . fmap toTxOut $ outs
    , txInfoFee = testFee conf
    , txInfoMint = foldMap (mintingToValue (testCurrencySymbol conf)) mints
    , txInfoDCert = []
    , txInfoWdrl = []
    , txInfoValidRange = testTimeRange conf
    , txInfoSignatories = toList pkhs
    , txInfoData =
        toList $
          mapMaybe toInputDatum ins
            <> mapMaybe toOutputDatum outs
            <> fmap datumWithHash dats
    , txInfoId = TxId "testTx"
    }
  where
    indexedInputs :: [(Integer, Input)]
    indexedInputs = zip [1 ..] . toList $ ins

mintingToValue :: CurrencySymbol -> Minting -> Value
mintingToValue cs = \case
  OwnMint tn i -> Value.singleton cs tn i
  OtherMint val -> val

toInputDatum :: Input -> Maybe (DatumHash, Datum)
toInputDatum (Input typ _) = case typ of
  ScriptInput _ dt -> Just . datumWithHash $ dt
  OwnInput dt -> Just . datumWithHash $ dt
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
    ScriptInput hash dat ->
      TxOut (scriptHashAddress hash) v . justDatumHash $ dat
    OwnInput dat ->
      TxOut (scriptHashAddress "") v . justDatumHash $ dat

justDatumHash :: BuiltinData -> Maybe DatumHash
justDatumHash = Just . datumHash . Datum

toTxOut :: Output -> TxOut
toTxOut (Output typ v) = case typ of
  PubKeyOutput pkh -> TxOut (pubKeyHashAddress pkh) v Nothing
  ScriptOutput hash dat ->
    TxOut (scriptHashAddress hash) v . justDatumHash $ dat
  OwnOutput dat ->
    TxOut (scriptHashAddress "") v . justDatumHash $ dat

walletPubKeyHash :: Wallet -> PubKeyHash
walletPubKeyHash = pubKeyHash . walletPubKey
