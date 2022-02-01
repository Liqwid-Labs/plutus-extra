module Test.Plutus.ScriptContext.Internal.Context (
  TransactionConfig (..),
  InputPosition (..),
  Purpose (..),
  UTXOType (..),
  ValueType (..),
  SideUTXO (..),
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
    txOutAddress,
    txOutDatumHash,
    txOutValue
  ),
  TxOutRef (TxOutRef),
  ValidatorHash,
  Value,
 )
import Plutus.V1.Ledger.Interval (Interval, always)
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
  , testInputPosition :: InputPosition
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
    , testInputPosition = Head
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
data SideUTXO = -- | @since
  SideUTXO
  { sUtxoType :: UTXOType
  , sUtxoValue :: ValueType
  }
  deriving stock
    ( -- | @since 1.0
      Show
    )

{- | An transaction input from the validated address, consisting of a value
 and a type. 'ValidatorInput' is a side input and it won't be validated.

 @since 1.0
-}
data ValidatorUTXO (datum :: Type) = -- | @since 1.0
  (ToData datum) =>
  ValidatorUTXO
  { vUtxoDatum :: datum
  , vUtxoValue :: Value
  }

-- | @since 1.0
deriving stock instance (Show d) => Show (ValidatorUTXO d)

{- | An transaction output to the validated address.

 @since 1.0
-}
data ValidatorUTXOs (p :: Purpose) where
  -- | @since 1.0
  NoValidatorUTXOs :: ValidatorUTXOs p
  -- | @since 1.0
  ValidatorUTXOs ::
    forall (datum :: Type) (redeemer :: Type).
    (FromData datum, ToData datum, Show datum) =>
    Map.Map Text (ValidatorUTXO datum) ->
    ValidatorUTXOs ( 'ForSpending datum redeemer)

-- | @since 1.0
deriving stock instance Show (ValidatorUTXOs p)

instance Semigroup (ValidatorUTXOs p) where
  NoValidatorUTXOs <> x = x
  x <> NoValidatorUTXOs = x
  (ValidatorUTXOs m1) <> (ValidatorUTXOs m2) = ValidatorUTXOs $ m1 <> m2

instance Monoid (ValidatorUTXOs p) where
  mempty = NoValidatorUTXOs

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

{- | Where to place the validated input in 'txInfoInputs' when generating a
 'ScriptContext'.

 @since 1.0
-}
data InputPosition = Head | Tail
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
    cbInputs :: Map.Map Text SideUTXO
  , -- | @since 1.0
    cbOutputs :: Map.Map Text SideUTXO
  , -- | @since 1.0
    cbSignatories :: Map.Map Text PubKeyHash
  , -- | @since 1.0
    cbDatums :: Map.Map Text BuiltinData
  , -- | @since 1.0
    cbMinting :: Map.Map Text Minting
  , -- | @since 1.0
    cbValidatorInputs :: ValidatorUTXOs p
  , -- | @since 1.0
    cbValidatorOutputs :: ValidatorUTXOs p
  }

-- | @since 1.0
deriving stock instance Show (ContextBuilder p)

-- | @since 1.0
instance Semigroup (ContextBuilder p) where
  {-# INLINEABLE (<>) #-}
  (ContextBuilder is os pkhs ds ms vis vos)
    <> (ContextBuilder is' os' pkhs' ds' ms' vis' vos') =
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
            { txInfoInputs = case testInputPosition conf of
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
        { txInfoInputs = createTxInInfos conf (Map.elems ins) vins
        , txInfoOutputs = createTxOuts conf (Map.elems outs) vouts
        , txInfoFee = testFee conf
        , txInfoMint = value
        , txInfoDCert = []
        , txInfoWdrl = []
        , txInfoValidRange = testTimeRange conf
        , txInfoSignatories = Map.elems pkhs
        , txInfoData =
            (validatorUtxosToDatum vins)
              <> (validatorUtxosToDatum vouts)
              <> (mapMaybe sideUtxoToDatum . Map.elems $ ins)
              <> (mapMaybe sideUtxoToDatum . Map.elems $ outs)
              <> (fmap datumWithHash . Map.elems $ dats)
        , txInfoId = TxId "testTx"
        }
  where
    unMint :: Minting -> Value
    unMint (Mint val) = val

    filterCS :: CurrencySymbol -> TokenName -> Integer -> Bool
    filterCS cs _ _ = cs /= testCurrencySymbol conf

sideUtxoToDatum :: SideUTXO -> Maybe (DatumHash, Datum)
sideUtxoToDatum (SideUTXO typ _) = case typ of
  ScriptUTXO _ dt -> Just . datumWithHash $ dt
  PubKeyUTXO _ dt -> datumWithHash <$> dt

validatorUtxosToDatum ::
  forall (p :: Purpose).
  ValidatorUTXOs p ->
  [(DatumHash, Datum)]
validatorUtxosToDatum = \case
  NoValidatorUTXOs -> []
  ValidatorUTXOs m ->
    map (\(ValidatorUTXO dt _) -> datumWithHash . toBuiltinData $ dt) $ Map.elems m

datumWithHash :: BuiltinData -> (DatumHash, Datum)
datumWithHash dt = (datumHash dt', dt')
  where
    dt' :: Datum
    dt' = Datum dt

sideUtxoToTxOut :: TransactionConfig -> SideUTXO -> TxOut
sideUtxoToTxOut conf (SideUTXO typ valType) =
  let val = extractValue conf valType
   in case typ of
        PubKeyUTXO pkh dat -> TxOut (pubKeyHashAddress pkh) val $ datumHash . Datum <$> dat
        ScriptUTXO hash dat ->
          TxOut (scriptHashAddress hash) val . justDatumHash $ dat

validatorUtxoToTxOut ::
  forall (d :: Type).
  TransactionConfig ->
  ValidatorUTXO d ->
  TxOut
validatorUtxoToTxOut conf (ValidatorUTXO dat val) =
  TxOut
    { txOutAddress = scriptHashAddress $ testValidatorHash conf
    , txOutValue = val
    , txOutDatumHash = justDatumHash $ toBuiltinData dat
    }

createTxInInfos ::
  forall (p :: Purpose).
  TransactionConfig ->
  [SideUTXO] ->
  ValidatorUTXOs p ->
  [TxInInfo]
createTxInInfos conf sideUtxos valUtxos =
  let allTxOuts = createTxOuts conf sideUtxos valUtxos
      txOutRefs = map (TxOutRef (testTxId conf)) [1 ..]
   in zipWith TxInInfo txOutRefs allTxOuts

createTxOuts ::
  forall (p :: Purpose).
  TransactionConfig ->
  [SideUTXO] ->
  ValidatorUTXOs p ->
  [TxOut]
createTxOuts conf sideUtxos valUtxos =
  let validatorHashAddress = scriptHashAddress $ testValidatorHash conf
      sideTxOuts =
        filter ((/= validatorHashAddress) . txOutAddress) $
          sideUtxoToTxOut conf <$> sideUtxos
      valTxOuts = case valUtxos of
        NoValidatorUTXOs -> []
        ValidatorUTXOs m -> fmap (validatorUtxoToTxOut conf) $ Map.elems m
   in valTxOuts <> sideTxOuts

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

extractValue :: TransactionConfig -> ValueType -> Value
extractValue conf = \case
  GeneralValue val -> val
  TokensValue tokenName pos ->
    Value.singleton
      (testCurrencySymbol conf)
      tokenName
      (getPositive pos)
