module Test.Tasty.Plutus.Internal.Context (
  Purpose (..),
  ExternalType (..),
  Input (..),
  Output (..),
  Minting (..),
  TransactionConfig (..),
  ContextBuilder (..),
  compileSpending,
  compileMinting,
) where

import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq)
import GHC.Exts (toList)
import Ledger.Scripts (datumHash)
import Plutus.V1.Ledger.Address (pubKeyHashAddress, scriptHashAddress)
import Plutus.V1.Ledger.Api (
  BuiltinData,
  CurrencySymbol,
  Datum (Datum),
  DatumHash,
  Interval,
  PubKeyHash,
  ScriptContext (ScriptContext),
  ScriptPurpose (Minting, Spending),
  ToData (toBuiltinData),
  TokenName,
  TxId (TxId),
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
  ValidatorHash,
  Value,
 )
import Plutus.V1.Ledger.Time (POSIXTime)
import Plutus.V1.Ledger.Value qualified as Value
import Test.Tasty.Plutus.Options (ScriptInputPosition (Head, Tail))

{- | Describes what kind of script this is meant to test. Directly
 corresponds to 'ScriptPurpose'.

 @since 1.0
-}
data Purpose
  = -- | Corresponds to 'Plutus.V1.Ledger.Contexts.Minting'. This tag applies
    -- to minting-policy-related structures and functions.
    --
    -- @since 1.0
    ForMinting
  | -- | Corresponds to 'Plutus.V1.Ledger.Contexts.Spending'. This tag applies
    -- to validator-related structures and functions.
    --
    -- @since 1.0
    ForSpending

{- | \'Marker type\' for input and output metadata.

 @since 3.0
-}
data ExternalType
  = -- | @since 3.0
    PubKeyType PubKeyHash
  | -- | @since 3.0
    ScriptType ValidatorHash BuiltinData
  | -- | @since 3.0mapMaybe
    OwnType BuiltinData
  deriving stock
    ( -- | @since 3.0
      Show
    )

{- | An input to a script, consisting of a value and a type.

 @since 1.0
-}
data Input
  = -- | @since 3.0
    Input ExternalType Value
  deriving stock
    ( -- | @since 1.0
      Show
    )

{- | An output from a script, consisting of a value and a type.

 @since 1.0
-}
data Output
  = -- | @since 3.0
    Output ExternalType Value
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

data TransactionConfig = TransactionConfig
  { testFee :: Value
  , testTimeRange :: Interval POSIXTime
  , testTxId :: TxId
  , testCurrencySymbol :: CurrencySymbol
  , testValidatorHash :: ValidatorHash
  , scriptInputPosition :: ScriptInputPosition
  }
  deriving stock (Show)

{- | A way to incrementally build up a script context.

 It is tagged with a 'Purpose' as a marker for what kind of script it's
 supposed to be validating.

 You can use the 'Semigroup' instance of this type to build up larger
 contexts. For example:

 > let cb = paysToWallet aWallet someValue <>
 >          signedWith aHash <>
 >          tagged aUniqueTag

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

-- | @since 3.4
instance Monoid (ContextBuilder p) where
  {-# INLINEABLE mempty #-}
  mempty = ContextBuilder mempty mempty mempty mempty mempty

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
          inInfo = createTxInInfo conf (0, Input (OwnType dt) val)
          inData = datumWithHash dt
       in baseInfo
            { txInfoInputs = case scriptInputPosition conf of
                Head -> inInfo : txInfoInputs baseInfo
                Tail -> txInfoInputs baseInfo <> [inInfo]
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

-- Helpers

baseTxInfo ::
  forall (p :: Purpose).
  TransactionConfig ->
  ContextBuilder p ->
  TxInfo
baseTxInfo conf (ContextBuilder ins outs pkhs dats mints) =
  let currSymb = testCurrencySymbol conf
      valHash = testValidatorHash conf
   in TxInfo
        { txInfoInputs = createTxInInfo conf <$> indexedInputs
        , txInfoOutputs = toList . fmap (toTxOut valHash) $ outs
        , txInfoFee = testFee conf
        , txInfoMint = foldMap (mintingToValue currSymb) mints
        , txInfoDCert = []
        , txInfoWdrl = []
        , txInfoValidRange = testTimeRange conf
        , txInfoSignatories = toList pkhs
        , txInfoData =
            (mapMaybe toInputDatum . toList $ ins)
              <> (mapMaybe toOutputDatum . toList $ outs)
              <> (toList . fmap datumWithHash $ dats)
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
  ScriptType _ dt -> Just . datumWithHash $ dt
  OwnType dt -> Just . datumWithHash $ dt
  PubKeyType _ -> Nothing

toOutputDatum :: Output -> Maybe (DatumHash, Datum)
toOutputDatum (Output typ _) = case typ of
  ScriptType _ dt -> Just . datumWithHash $ dt
  OwnType dt -> Just . datumWithHash $ dt
  PubKeyType _ -> Nothing

datumWithHash :: BuiltinData -> (DatumHash, Datum)
datumWithHash dt = (datumHash dt', dt')
  where
    dt' :: Datum
    dt' = Datum dt

createTxInInfo :: TransactionConfig -> (Integer, Input) -> TxInInfo
createTxInInfo conf (ix, Input typ v) =
  TxInInfo (TxOutRef (testTxId conf) ix) $ case typ of
    PubKeyType pkh -> TxOut (pubKeyHashAddress pkh) v Nothing
    ScriptType hash dat ->
      TxOut (scriptHashAddress hash) v . justDatumHash $ dat
    OwnType dat ->
      TxOut (scriptHashAddress . testValidatorHash $ conf) v . justDatumHash $ dat

justDatumHash :: BuiltinData -> Maybe DatumHash
justDatumHash = Just . datumHash . Datum

toTxOut :: ValidatorHash -> Output -> TxOut
toTxOut valHash (Output typ v) = case typ of
  PubKeyType pkh -> TxOut (pubKeyHashAddress pkh) v Nothing
  ScriptType hash dat ->
    TxOut (scriptHashAddress hash) v . justDatumHash $ dat
  OwnType dat ->
    TxOut (scriptHashAddress valHash) v . justDatumHash $ dat
