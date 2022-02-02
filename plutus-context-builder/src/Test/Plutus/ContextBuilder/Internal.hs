module Test.Plutus.ContextBuilder.Internal (
  -- * Types
  TransactionConfig (..),
  InputPosition (..),
  Purpose (..),
  UTXOType (..),
  ValueType (..),
  SideUTXO (..),
  ValidatorUTXO (..),
  ValidatorUTXOs (..),
  TestUTXO (..),
  Minting (..),
  ContextBuilder (..),

  -- * Utilities functions
  buildSpending,
  buildMinting,
  defTransactionConfig,
  buildSpendingDef,
  buildMintingDef,
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
import Test.Plutus.ContextBuilder.Minting (
  MintingPolicyAction (BurnAction, MintAction),
  MintingPolicyTask (MPTask),
  Tokens (Tokens),
 )
import Prelude hiding (length)

{- Config with the parameters necessary to build the context.

 @since 1.0
-}
data TransactionConfig = TransactionConfig
  { testFee :: Value
  , testTimeRange :: Interval POSIXTime
  , testTxId :: TxId
  , testCurrencySymbol :: CurrencySymbol
  , testValidatorHash :: ValidatorHash
  , testInputPosition :: InputPosition
  }
  deriving stock (Show)

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

{- 'TransactionConfig' passed by default to 'ScriptContext'.

 @since 1.0
-}
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

{- | Represents metadata of UTxO at different types of address.

 @since 1.0
-}
data UTXOType
  = -- | Metadata of an UTxO at a 'PubKeyHash' address
    -- | @since 1.0
    PubKeyUTXO PubKeyHash (Maybe BuiltinData)
  | -- | @since 1.0
    -- | Metadata of an UTxO at a 'ValidatorHash' address
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

{- | An UTxO involved in a transaction as an input or output.

 = Note

 'SideUTXO' must not be used to represent a UTxO at the address of
 the tested validator. To do this, use 'ValidatorUTXO' or 'TestUTXO'.
 When constructing a context, SideUTXO with the address that matches
 the 'testValidatorAddress' in the 'TransactionConfig' will be discarded.

 @since 1.0
-}
data SideUTXO =
  SideUTXO
  { sUtxoType :: UTXOType
  , sUtxoValue :: ValueType
  }
  deriving stock
    ( -- | @since 1.0
      Show
    )

{- | An UTxO at the tested validator address.
This UTxO won't be used as 'Spending' in the 'ScriptPurpose'
of the builded 'ScriptContext'. For the representation of a 'Spending' UTxO
use 'TestUTXO'.

 @since 1.0
-}
data ValidatorUTXO (datum :: Type) = (ToData datum) =>
  ValidatorUTXO
  { vUtxoDatum :: datum
  , vUtxoValue :: Value
  }

-- | @since 1.0
deriving stock instance (Show d) => Show (ValidatorUTXO d)

{- | UTxO at the tested validator address. It will be used as 'Spending'
 in the 'ScriptPurpose' of the builded 'ScriptContext'.

 @since 1.0
-}
data TestUTXO (datum :: Type) = (ToData datum) =>
  TestUTXO
  { tUtxoDatum :: datum
  , tUtxoValue :: Value
  }

-- | @since 1.0
deriving stock instance (Show d) => Show (TestUTXO d)

{- | ValidatorUTXOs represents a set of 'ValidatorUTXO's.
 It is type compatible with 'ContextBuider'.

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

-- | @since 1.0
instance Semigroup (ValidatorUTXOs p) where
  NoValidatorUTXOs <> x = x
  x <> NoValidatorUTXOs = x
  (ValidatorUTXOs m1) <> (ValidatorUTXOs m2) = ValidatorUTXOs $ m1 <> m2

-- | @since 1.0
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

{- | A way to incrementally build up a script context.

 It is tagged with a 'Purpose' as a marker for what kind of 'ScriptPurpose'
 will be used in the constructed 'ScriptContext'.

 You can use the 'Semigroup' instance of this type to build up larger
 contexts. For example:

 > let cb = paysToWallet "userWalletOut" aWallet someValue <>
 >          signedWith "userSignature" aHash <>
 >          tagged "specialTag" aUniqueTag

 @since 1.0
-}
data ContextBuilder (p :: Purpose) = ContextBuilder
  { -- | @since 1.0
    cbInputs :: Map.Map Text SideUTXO
  , -- | @since 1.0
    cbOutputs :: Map.Map Text SideUTXO
  , -- | @since 1.0
    cbSignatures :: Map.Map Text PubKeyHash
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

{- | 'buildSpending' with the default transaction config 'defTransactionConfig'

 @since 1.0
-}
buildSpendingDef ::
  forall (datum :: Type) (redeemer :: Type).
  ContextBuilder ( 'ForSpending datum redeemer) ->
  TestUTXO datum ->
  ScriptContext
buildSpendingDef = buildSpending defTransactionConfig

{- | Construct 'ScriptContext' with 'Spending' 'ScriptPurpose' from the provided blocks.

 @since 1.0
-}
buildSpending ::
  forall (datum :: Type) (redeemer :: Type).
  TransactionConfig ->
  ContextBuilder ( 'ForSpending datum redeemer) ->
  TestUTXO datum ->
  ScriptContext
buildSpending conf cb (TestUTXO d v) =
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

{- | 'buildMinting' with the default transaction config 'defTransactionConfig'

 @since 1.0
-}
buildMintingDef ::
  forall (r :: Type).
  ContextBuilder ( 'ForMinting r) ->
  NonEmpty MintingPolicyTask ->
  ScriptContext
buildMintingDef = buildMinting defTransactionConfig

{- | Construct 'ScriptContext' with 'Minting' 'ScriptPurpose' from the provided blocks.

 @since 1.0
-}
buildMinting ::
  forall (r :: Type).
  TransactionConfig ->
  ContextBuilder ( 'ForMinting r) ->
  NonEmpty MintingPolicyTask ->
  ScriptContext
buildMinting conf cb toks =
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
      insLst = filter (checkSideUtxoAddress conf) . Map.elems $ ins
      outsLst = filter (checkSideUtxoAddress conf) . Map.elems $ outs
   in TxInfo
        { txInfoInputs = createTxInInfos conf insLst vins
        , txInfoOutputs = createTxOuts conf outsLst vouts
        , txInfoFee = testFee conf
        , txInfoMint = value
        , txInfoDCert = []
        , txInfoWdrl = []
        , txInfoValidRange = testTimeRange conf
        , txInfoSignatories = Map.elems pkhs
        , txInfoData =
            validatorUtxosToDatum vins
              <> validatorUtxosToDatum vouts
              <> mapMaybe sideUtxoToDatum insLst
              <> mapMaybe sideUtxoToDatum outsLst
              <> (fmap datumWithHash . Map.elems $ dats)
        , txInfoId = TxId "testTx"
        }
  where
    unMint :: Minting -> Value
    unMint (Mint val) = val

    filterCS :: CurrencySymbol -> TokenName -> Integer -> Bool
    filterCS cs _ _ = cs /= testCurrencySymbol conf

checkSideUtxoAddress :: TransactionConfig -> SideUTXO -> Bool
checkSideUtxoAddress conf (SideUTXO typ _) =
  let sideAddress = case typ of
        PubKeyUTXO pkh _ -> pubKeyHashAddress pkh
        ScriptUTXO hash _ -> scriptHashAddress hash
   in sideAddress /= scriptHashAddress (testValidatorHash conf)

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
  let sideTxOuts = sideUtxoToTxOut conf <$> sideUtxos
      valTxOuts = case valUtxos of
        NoValidatorUTXOs -> []
        ValidatorUTXOs m -> fmap (validatorUtxoToTxOut conf) $ Map.elems m
   in valTxOuts <> sideTxOuts

createOwnTxInInfo :: TransactionConfig -> BuiltinData -> Value -> TxInInfo
createOwnTxInInfo conf dat val =
  TxInInfo
    { txInInfoOutRef = TxOutRef (testTxId conf) 0
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
