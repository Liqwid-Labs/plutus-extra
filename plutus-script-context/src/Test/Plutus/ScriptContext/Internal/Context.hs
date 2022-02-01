module Test.Plutus.ScriptContext.Internal.Context (
  TransactionConfig(..),
  Purpose (..),
  UTXOType (..),
  ValueType (..),
  Input (..),
  Output (..),
  Minting (..),
  ContextBuilder (..),
  defTransactionConfig,
  compileSpending,
  compileMinting,
) where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Ledger.Scripts (datumHash)
import Plutus.V1.Ledger.Interval (Interval, always)
import Plutus.V1.Ledger.Address (pubKeyHashAddress, scriptHashAddress)
import Plutus.V1.Ledger.Api (
  BuiltinData,
  CurrencySymbol,
  Datum (Datum),
  DatumHash,
  FromData,
  PubKeyHash,
  ScriptContext (ScriptContext),
  ScriptPurpose (Minting, Spending),
  ToData (toBuiltinData),
  TokenName,
  TxId (TxId),
  TxInInfo (
    TxInInfo,
    txInInfoOutRef,
    txInInfoResolved
  ),
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
  TxOut (
    TxOut,
    txOutValue,
    txOutDatumHash,
    txOutAddress
  ),
  TxOutRef (TxOutRef),
  ValidatorHash,
  Value,
 )
import Plutus.V1.Ledger.Time (POSIXTime)
import Plutus.V1.Ledger.Value qualified as Value
import Plutus.V1.Ledger.Value.Extra (filterValue)
import PlutusTx.Positive (Positive, getPositive)
import Test.Plutus.ScriptContext.Internal.Minting (
  MintingPolicyAction (BurnAction, MintAction),
  MintingPolicyTask (MPTask),
  Tokens (Tokens),
 )
import Prelude hiding (length)

data TransactionConfig = TransactionConfig
  { testFee :: Value
  , testTimeRange :: Interval POSIXTime
  , testTxId :: TxId
  , testCurrencySymbol :: CurrencySymbol
  , testValidatorHash :: ValidatorHash
  , scriptInputPosition :: ScriptInputPosition
  }
  deriving stock (Show)

defTransactionConfig :: TransactionConfig
defTransactionConfig =
  TransactionConfig
    { testFee = mempty
    , testTimeRange = always
    , testTxId = TxId "abcd"
    , testCurrencySymbol = "ff"
    , testValidatorHash = "90ab"
    , scriptInputPosition = Head
    }

{- | Describes what kind of script this is meant to test. Directly
 corresponds to 'ScriptPurpose'.

 @since 1.0
-}
data Purpose where
  -- | Corresponds to 'Plutus.V1.Ledger.Contexts.Minting'. This tag applies
  -- to minting-policy-related structures and functions.
  --
  -- @since 1.0
  ForMinting ::
    forall redeemer.
    -- | @since 1.0
    redeemer ->
    Purpose
  -- | Corresponds to 'Plutus.V1.Ledger.Contexts.Spending'. This tag applies
  -- to validator-related structures and functions.
  --
  -- @since 1.0
  ForSpending ::
    forall datum redeemer.
    -- | @since 1.0
    datum ->
    -- | @since 1.0
    redeemer ->
    Purpose

{- | \'Marker type\' for input and output metadata.

 @since 1.0
-}
data UTXOType
  = -- | @since 1.0
    PubKeyUTXO PubKeyHash (Maybe BuiltinData)
  | -- | @since 1.0
    ScriptUTXO ValidatorHash BuiltinData
  deriving stock
    ( -- | @since 1.0
      Show
    )

{- | Different types of value:

  'TokensValue' is only used within @ContextBuilder ('ForMinting r)@
  for representing classes, belonging to the tested MintingPolicy.

  In all other cases 'GeneralValue' is used.

 @since 1.0
-}
data ValueType
  = -- | @since 1.0
    GeneralValue Value
  | -- | @since 1.0
    TokensValue TokenName Positive
  deriving stock
    ( -- | @since 1.0
      Eq
    , -- | @since 1.0
      Show
    )

{- | An transaction input, consisting of a value and a type.

 @since 1.0
-}
data Input =
  -- | @since
  Input
    { inType :: UTXOType
    , inValue :: ValueType
    }
  deriving stock
    ( -- | @since 1.0
      Show
    )

{- | An transaction input from the validated address, consisting of a value
 and a type. 'ValidatorInput' is a side input and it will not be validated.

 @since 1.0
-}
data ValidatorInput (p :: Purpose) where
  -- | @since 1.0
  ValidatorInput ::
    forall (datum :: Type) (redeemer :: Type).
    (FromData datum, ToData datum) =>
    datum ->
    Value ->
    ValidatorInput ( 'ForSpending datum redeemer) 

-- | @since 1.0
deriving stock instance
  (Show datum) => Show (ValidatorInput ( 'ForSpending datum redeemer))

{- | An transaction output, consisting of a value and a type.

 @since 1.0
-}
data Output =
  -- | @since 1.0 
  Output
    { outType :: UTXOType
    , outValue :: ValueType
    }
  deriving stock
    ( -- | @since 1.0
      Show
    )

{- | An transaction output to the validated address.

 @since 1.0
-}
data ValidatorOutput (p :: Purpose) where
  -- | @since 1.0
  ValidatorOutput ::
    forall (datum :: Type) (redeemer :: Type).
    (FromData datum, ToData datum) =>
    datum ->
    Value ->
    ValidatorOutput ( 'ForSpending datum redeemer)

-- | @since 1.0
deriving stock instance (Show datum) =>
  Show (ValidatorOutput ( 'ForSpending datum redeemer))

{- | A 'Value' minted with a minting policy other than the one being tested.
 Do not use this for tokens being minted by the tested minting policy.

 = Note

 Asset classes with 'CurrencySymbol' matching 'testCurrencySymbol' in 'TransactionConfig'
 will be excluded from the resulting 'ScriptContext'.

 @since 1.0
-}
data Minting
  = -- | @since 1.0
    Mint Value
  deriving stock
    ( -- | @since 1.0
      Show
    )

{- | Where to place the script input in 'txInfoInputs' when generating a
 'ScriptContext'.

 @since 1.0
-}
data ScriptInputPosition = Head | Tail
  deriving stock
    ( -- | @since 1.0
      Eq
    , -- | @since 1.0
      Show
    )

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
data ContextBuilder (p :: Purpose) = ContextBuilder
  { -- | @since 1.0
    cbInputs :: Map.Map Text Input
  , -- | @since 1.0
    cbOutputs :: Map.Map Text Output
  , -- | @since 1.0
    cbSignatories :: Map.Map Text PubKeyHash
  , -- | @since 1.0
    cbDatums :: Map.Map Text BuiltinData
  , -- | @since 1.0
    cbMinting :: Map.Map Text Minting
  , cbValidatorInputs :: Maybe (Map.Map Text (ValidatorInput p))
  , cbValidatorOutputs :: Maybe (Map.Map Text (ValidatorOutput p))
  }

-- | @since 1.0
deriving stock instance
  (Show (ValidatorInput p), Show (ValidatorOutput p)) =>
  Show (ContextBuilder p)

-- | @since 1.0
instance Semigroup (ContextBuilder p) where
  {-# INLINEABLE (<>) #-}
  (ContextBuilder is os pkhs ds ms vis vos) <>
    (ContextBuilder is' os' pkhs' ds' ms' vis' vos') =
      ContextBuilder
        (is <> is')
        (os <> os')
        (pkhs <> pkhs')
        (ds <> ds')
        (ms <> ms')
        (vis <> vis')
        (vos <> vos')

-- | @since 1.0
instance Monoid (ContextBuilder p) where
  {-# INLINEABLE mempty #-}
  mempty = ContextBuilder mempty mempty mempty mempty mempty mempty mempty

compileSpending ::
  forall (datum :: Type) (redeemer :: Type).
  (ToData datum) =>
  TransactionConfig ->
  ContextBuilder ( 'ForSpending datum redeemer) ->
  datum ->
  Value ->
  ScriptContext
compileSpending conf cb d v =
  ScriptContext go
    . Spending
    . TxOutRef (testTxId conf)
    $ 0
  where
    go :: TxInfo
    go =
      let dt = toBuiltinData d
          baseInfo = baseTxInfo conf cb
          inInfo = createOwnTxInInfo conf dt v
          inData = datumWithHash dt
       in baseInfo
            { txInfoInputs = case scriptInputPosition conf of
                Head -> inInfo : txInfoInputs baseInfo
                Tail -> txInfoInputs baseInfo <> [inInfo]
            , txInfoData = inData : txInfoData baseInfo
            }

compileMinting ::
  forall (r :: Type).
  TransactionConfig ->
  ContextBuilder ( 'ForMinting r) ->
  NonEmpty MintingPolicyTask ->
  ScriptContext
compileMinting conf cb toks =
  ScriptContext go (Minting sym)
  where
    sym :: CurrencySymbol
    sym = testCurrencySymbol conf

    mintingValue :: Value
    mintingValue =
      Map.foldMapWithKey (Value.singleton sym)
        . Map.fromList
        . map processMPTask
        . NonEmpty.toList
        $ toks

    go :: TxInfo
    go =
      let baseInfo = baseTxInfo conf cb
       in baseInfo
            { txInfoMint =
                mintingValue <> txInfoMint baseInfo
            }
    processMPTask :: MintingPolicyTask -> (TokenName, Integer)
    processMPTask (MPTask action (Tokens tn pos)) =
      let i = case action of
            MintAction -> getPositive pos
            BurnAction -> negate $ getPositive pos
       in (tn, i)

-- Helpers

baseTxInfo ::
  forall (p :: Purpose).
  TransactionConfig ->
  ContextBuilder p ->
  TxInfo
baseTxInfo conf (ContextBuilder ins outs pkhs dats mints vins vouts) =
  let value = foldMap (filterValue filterCS . unMint) mints
   in TxInfo
        { txInfoInputs = createTxInInfos conf (Map.elems ins) (fmap Map.elems vins)
        , txInfoOutputs = createTxOutInfos conf (Map.elems outs) (fmap Map.elems vouts)
        , txInfoFee = testFee conf
        , txInfoMint = value
        , txInfoDCert = []
        , txInfoWdrl = []
        , txInfoValidRange = testTimeRange conf
        , txInfoSignatories = Map.elems pkhs
        , txInfoData =
            (validatorInputsToDatum vins)
              <> (validatorOutputsToDatum vouts)
              <> (mapMaybe inputToDatum . Map.elems $ ins)
              <> (mapMaybe outputToDatum . Map.elems $ outs)
              <> (fmap datumWithHash . Map.elems $ dats)
        , txInfoId = TxId "testTx"
        }
  where
    unMint :: Minting -> Value
    unMint (Mint val) = val

    filterCS :: CurrencySymbol -> TokenName -> Integer -> Bool
    filterCS cs _ _ = cs /= testCurrencySymbol conf

inputToDatum :: Input -> Maybe (DatumHash, Datum)
inputToDatum (Input typ _) = case typ of
  ScriptUTXO _ dt -> Just . datumWithHash $ dt
  PubKeyUTXO _ dt -> datumWithHash <$> dt

validatorInputsToDatum ::
  forall (p :: Purpose).
  Maybe (Map.Map Text (ValidatorInput p)) -> [(DatumHash, Datum)]
validatorInputsToDatum Nothing = []
validatorInputsToDatum (Just m) =
  map (\(ValidatorInput dt _) -> datumWithHash . toBuiltinData $ dt) $ Map.elems m

validatorOutputsToDatum ::
  forall (p :: Purpose).
  Maybe (Map.Map Text (ValidatorOutput p)) ->
  [(DatumHash, Datum)]
validatorOutputsToDatum Nothing = []
validatorOutputsToDatum (Just m) =
  map (\(ValidatorOutput dt _) -> datumWithHash . toBuiltinData $ dt) $ Map.elems m

outputToDatum :: Output -> Maybe (DatumHash, Datum)
outputToDatum (Output typ _) = case typ of
  ScriptUTXO _ dt -> Just . datumWithHash $ dt
  PubKeyUTXO _ dt -> datumWithHash <$> dt

datumWithHash :: BuiltinData -> (DatumHash, Datum)
datumWithHash dt = (datumHash dt', dt')
  where
    dt' :: Datum
    dt' = Datum dt

inputToTxOut :: TransactionConfig -> Input -> TxOut
inputToTxOut conf (Input typ valType) =
  let val = extractValue conf valType
   in case typ of
        PubKeyUTXO pkh dat -> TxOut (pubKeyHashAddress pkh) val $ datumHash . Datum <$> dat
        ScriptUTXO hash dat ->
          TxOut (scriptHashAddress hash) val . justDatumHash $ dat

validatorInputToTxOut ::
  forall (p :: Purpose).
  TransactionConfig ->
  ValidatorInput p ->
  TxOut
validatorInputToTxOut conf = \case
  ValidatorInput dat val ->
    TxOut
      { txOutAddress = scriptHashAddress $ testValidatorHash conf
      , txOutValue = val
      , txOutDatumHash = justDatumHash $ toBuiltinData dat
      }

createTxInInfos ::
  forall (p :: Purpose).
  TransactionConfig ->
  [Input] ->
  Maybe [ValidatorInput p] ->
  [TxInInfo]
createTxInInfos conf ins mb = 
  let validatorHashAddress = scriptHashAddress $ testValidatorHash conf
      txOuts = 
        filter ((/= validatorHashAddress) . txOutAddress)
          $ inputToTxOut conf <$> ins
      mbTxOuts = maybe [] (fmap $ validatorInputToTxOut conf) mb
      allTxOuts = mbTxOuts <> txOuts
      txOutRefs = map (TxOutRef (testTxId conf)) [1..]
   in zipWith TxInInfo txOutRefs allTxOuts

createOwnTxInInfo :: TransactionConfig -> BuiltinData -> Value -> TxInInfo
createOwnTxInInfo conf dat val =
  TxInInfo
    { txInInfoOutRef = (TxOutRef (testTxId conf) 0)
    , txInInfoResolved = 
        TxOut
          { txOutAddress = scriptHashAddress $ testValidatorHash conf
          , txOutValue = val
          , txOutDatumHash = justDatumHash dat
          }
    }

justDatumHash :: BuiltinData -> Maybe DatumHash
justDatumHash = Just . datumHash . Datum

outputToTxOut :: TransactionConfig -> Output -> TxOut
outputToTxOut conf (Output typ valType) =
  let val = extractValue conf valType
   in case typ of
        PubKeyUTXO pkh dat ->
          TxOut (pubKeyHashAddress pkh) val (datumHash . Datum <$> dat)
        ScriptUTXO hash dat ->
          TxOut (scriptHashAddress hash) val (justDatumHash dat)

validatorOutputToTxOut ::
  forall (p :: Purpose).
  TransactionConfig ->
  ValidatorOutput p ->
  TxOut
validatorOutputToTxOut conf = \case
  ValidatorOutput dat val ->
    TxOut
      { txOutAddress = scriptHashAddress $ testValidatorHash conf
      , txOutValue = val
      , txOutDatumHash = justDatumHash $ toBuiltinData dat
      }

createTxOutInfos ::
  forall (p :: Purpose).
  TransactionConfig ->
  [Output] ->
  Maybe [ValidatorOutput p] ->
  [TxOut]
createTxOutInfos conf outs mb =
  let validatorHashAddress = scriptHashAddress $ testValidatorHash conf
      txOuts =
        filter ((/= validatorHashAddress) . txOutAddress)
          $ outputToTxOut conf <$> outs
      mbTxOuts = maybe [] (fmap $ validatorOutputToTxOut conf) mb
   in mbTxOuts <> txOuts

extractValue :: TransactionConfig -> ValueType -> Value
extractValue conf = \case
  GeneralValue val -> val
  TokensValue tokenName pos ->
    Value.singleton
      (testCurrencySymbol conf)
      tokenName
      (getPositive pos)
