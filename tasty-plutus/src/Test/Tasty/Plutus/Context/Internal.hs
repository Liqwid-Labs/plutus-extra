-- Internals of context building.
module Test.Tasty.Plutus.Context.Internal (
  Purpose (..),
  ExternalType (..),
  Input (..),
  Output (..),
  Minting (..),
  ContextBuilder (..),
  TransactionConfig (..),
  compileSpending,
  compileMinting,
) where

import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq)
import GHC.Exts (toList)
import Ledger.Address (pubKeyHashAddress, scriptHashAddress)
import Ledger.Crypto (PubKeyHash)
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
import Plutus.V1.Ledger.Time (POSIXTime)
import Plutus.V1.Ledger.TxId (TxId (TxId))
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx.Builtins (BuiltinData)
import PlutusTx.IsData.Class (ToData (toBuiltinData))

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

{- | \'Marker type\' for input and output metadata.

 @since 3.0
-}
data ExternalType
  = -- | @since 3.0
    PubKeyType PubKeyHash
  | -- | @since 3.0
    ScriptType ValidatorHash BuiltinData
  | -- | @since 3.0
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
          inInfo =
            createTxInInfo
              conf
              (0, Input (OwnType dt) val)
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
