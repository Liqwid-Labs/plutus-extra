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
  spendingScriptContext,
  mintingScriptContext,
  defTransactionConfig,
  spendingScriptContextDef,
  mintingScriptContextDef,
  makeIncompleteContexts,
) where

import Control.Arrow ((***))
import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Semigroup (sconcat)
import Data.Set (Set)
import Data.Set qualified as Set
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

import PlutusTx.Prelude (length)
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
data SideUTXO = SideUTXO
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

 @since 2.0
-}
data ValidatorUTXO (datum :: Type) = ValidatorUTXO
  { vUtxoDatum :: datum
  , vUtxoValue :: Value
  }
  deriving stock
    ( -- | @since 1.0
      Show
    )

{- | UTxO at the tested validator address. It will be used as 'Spending'
 in the 'ScriptPurpose' of the builded 'ScriptContext'.

 @since 2.0
-}
data TestUTXO (datum :: Type) = TestUTXO
  { tUtxoDatum :: datum
  , tUtxoValue :: Value
  }
  deriving stock
    ( -- | @since 1.0
      Show
    )

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
  , -- | @since 2.0
    cbSignatures :: Map.Map Text (Set PubKeyHash)
  , -- | @since 1.0
    cbDatums :: Map.Map Text BuiltinData
  , -- | @since 1.0
    cbMinting :: Map.Map Text Minting
  , -- | @since 1.0
    cbValidatorInputs :: ValidatorUTXOs p
  , -- | @since 1.0
    cbValidatorOutputs :: ValidatorUTXOs p
  }
  deriving stock
    ( -- | @since 1.0
      Show
    )

-- | @since 2.0
instance Semigroup (ContextBuilder p) where
  {-# INLINEABLE (<>) #-}
  (ContextBuilder is os pkhs ds ms vis vos)
    <> (ContextBuilder is' os' pkhs' ds' ms' vis' vos') =
      ContextBuilder
        (is <> is')
        (os <> os')
        (Map.unionWith Set.union pkhs pkhs')
        (ds <> ds')
        (ms <> ms')
        (vis <> vis')
        (vos <> vos')

-- | @since 1.0
instance Monoid (ContextBuilder p) where
  {-# INLINEABLE mempty #-}
  mempty = ContextBuilder mempty mempty mempty mempty mempty mempty mempty

{- | As 'spendingScriptContext', but with the default
transaction config 'defTransactionConfig'.

 @since 2.0
-}
spendingScriptContextDef ::
  forall (datum :: Type) (redeemer :: Type).
  (ToData datum) =>
  ContextBuilder ( 'ForSpending datum redeemer) ->
  TestUTXO datum ->
  ScriptContext
spendingScriptContextDef = spendingScriptContext defTransactionConfig

{- | Construct 'ScriptContext' with 'Spending' 'ScriptPurpose' from the provided blocks.

 @since 2.0
-}
spendingScriptContext ::
  forall (datum :: Type) (redeemer :: Type).
  (ToData datum) =>
  TransactionConfig ->
  ContextBuilder ( 'ForSpending datum redeemer) ->
  TestUTXO datum ->
  ScriptContext
spendingScriptContext conf cb (TestUTXO d v) =
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

{- | As 'mintingScriptContext', but with the default
transaction config 'defTransactionConfig'.

 @since 1.0
-}
mintingScriptContextDef ::
  forall (r :: Type).
  ContextBuilder ( 'ForMinting r) ->
  NonEmpty MintingPolicyTask ->
  ScriptContext
mintingScriptContextDef = mintingScriptContext defTransactionConfig

{- | Construct 'ScriptContext' with 'Minting' 'ScriptPurpose' from the provided blocks.

 @since 1.0
-}
mintingScriptContext ::
  forall (r :: Type).
  TransactionConfig ->
  ContextBuilder ( 'ForMinting r) ->
  NonEmpty MintingPolicyTask ->
  ScriptContext
mintingScriptContext conf cb toks =
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

{- | Combine a list of partial contexts that should,
     when combined, validate, but fail when any one
     partial context is missing. The input is a list
     of pairs where the first element is the partial
     context, and the second is the test message when
     that particular context is missing. e.g.

     > makeIncompleteContexts
     >   [ (context1, "Missing context 1")
     >   , (context2, "Missing context 2")
     >   , (context3, "Missing context 3")
     >   ]

     is equivalent to

     > [ (context2 <> context3, "Missing context 1")
     > , (context1 <> context3, "Missing context 2")
     > , (context1 <> context2, "Missing context 3")
     > ]

     This can then be run in a `withTestScript` block
     like so:

     > mapM_ (\(ctx,str) -> shouldn'tValidate str input ctx) convertedContexts

     This works on any `Semigroup` instead of just `ContextBuilder`.

 @since 1.0
-}
makeIncompleteContexts ::
  forall (s :: Type).
  (Semigroup s) =>
  [(s, String)] ->
  [(s, String)]
makeIncompleteContexts ctxs = map (first sconcat) ctxs2
  where
    ctxs2 = mapMaybe nonEmpty1st $ removeContext ctxs
    nonEmpty1st :: ([a], b) -> Maybe (NonEmpty a, b)
    nonEmpty1st (xs, y) = (,y) <$> NonEmpty.nonEmpty xs

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
        , txInfoSignatories = concatMap Set.toList . Map.elems $ pkhs
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
  (ToData d) =>
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

-- Helper for makeIncompleteContexts : Take a list
-- of pairs, and create another list of pairs where
-- the first element is the concatenation of every
-- element but the n-th, and the second element is
-- the second element of the pair that was removed.
removeContext :: [(a, b)] -> [([a], b)]
removeContext xs =
  mapMaybe
    (maybe2nd . (map fst *** fmap snd) . (`removeNth` xs))
    [0 .. (length xs - 1)]
  where
    maybe2nd :: (a, Maybe b) -> Maybe (a, b)
    maybe2nd (x, y) = (x,) <$> y

-- Remove and return the n-th element of a list.
removeNth :: Integer -> [a] -> ([a], Maybe a)
removeNth _ [] = ([], Nothing)
removeNth 0 (x : xs) = (xs, Just x)
removeNth n (x : xs) = first (x :) $ removeNth (n - 1) xs
