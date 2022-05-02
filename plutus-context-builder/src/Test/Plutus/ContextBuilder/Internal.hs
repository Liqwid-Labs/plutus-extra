{-# LANGUAGE NamedFieldPuns #-}

module Test.Plutus.ContextBuilder.Internal (
  -- * Types
  TestScript (
    TestValidator,
    getTestValidator,
    getTestValidatorCode,
    TestMintingPolicy,
    getTestMintingPolicy,
    getTestMintingPolicyCode
  ),
  TransactionConfig (..),
  InputPosition (..),
  Purpose (..),
  UTXOType (..),
  ValueType (..),
  SideUTXO (..),
  SomeValidatedUTXO (..),
  ValidatorUTXO (..),
  ValidatorUTXOs (..),
  TestUTXO (..),
  Minting (..),
  Naming (..),
  ContextBuilder (..),
  ContextFragment (..),

  -- * Utilities functions
  spendingScriptContext,
  mintingScriptContext,
  defTransactionConfig,
  spendingScriptContextDef,
  mintingScriptContextDef,
  makeIncompleteContexts,
  foldBuilt,
  transactionSpending,
  transactionMinting,
) where

import Control.Arrow ((***))
import Data.Bifunctor (first)
import Data.Foldable (fold)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Semigroup (sconcat)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Exts (toList)
import Ledger (scriptAddress)
import Ledger.Scripts (datumHash, mintingPolicyHash, validatorHash)
import Plutus.V1.Ledger.Address (pubKeyHashAddress, scriptHashAddress)
import Plutus.V1.Ledger.Api (
  BuiltinData,
  CurrencySymbol,
  Datum (Datum),
  DatumHash,
  FromData,
  MintingPolicy,
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
  Validator,
  ValidatorHash,
  Value,
 )
import Plutus.V1.Ledger.Interval (Interval, always)
import Plutus.V1.Ledger.Time (POSIXTime)
import Plutus.V1.Ledger.Value qualified as Value
import Plutus.V1.Ledger.Value.Extra (filterValue)
import PlutusTx (CompiledCode)
import PlutusTx.Positive (Positive, getPositive)
import Test.Plutus.ContextBuilder.Minting (
  MintingPolicyAction (BurnAction, MintAction),
  MintingPolicyTask (MPTask),
  Tokens (Tokens),
 )

import PlutusTx.Prelude (length)
import Prelude hiding (length)

{- | Typed wrapper for the 'Validator' and 'MintingPolicy' used to match
 the datum and redeemer types of the 'Validator' and the data passed to it.

 We don't expose constructors. To create a 'TestScript', use helper functions,
 such as 'mkTestValidator' and 'mkTestMintingPolicy'. In case you intend
 to test something tricky, you can use 'mkTestValidatorUnsafe'
 and 'mkTestMintingPolicyUnsafe' to create a 'TestScript'
 that accepts a datum and/or redeemer inconsistent with its internal type.

 @since 6.0
-}
data TestScript (p :: Purpose) where
  -- | since 6.0
  TestValidator ::
    forall (d :: Type) (r :: Type) (code :: Type).
    { getTestValidatorCode :: CompiledCode code
    , getTestValidator :: Validator
    } ->
    TestScript ( 'ForSpending d r)
  -- | since 6.0
  TestMintingPolicy ::
    forall (r :: Type) (code :: Type).
    { getTestMintingPolicyCode :: CompiledCode code
    , getTestMintingPolicy :: MintingPolicy
    } ->
    TestScript ( 'ForMinting r)

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
  -- | This tag applies to whole-transaction testing.
  --
  -- @since 2.1
  ForTransaction :: Purpose

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
of the built 'ScriptContext'. For the representation of a 'Spending' UTxO
use 'TestUTXO'.

 @since 2.0
-}
data ValidatorUTXO (datum :: Type) = ValidatorUTXO
  { vUtxoDatum :: datum
  , vUtxoValue :: Value
  }
  deriving stock
    ( -- | @since 2.1
      Eq
    , -- | @since 1.0
      Show
    )

{- | UTxO at the tested validator address. It will be used as 'Spending'
 in the 'ScriptPurpose' of the built 'ScriptContext'.

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
  -- | @since 2.1
  MultiValidatorUTXOs ::
    Map.Map Text SomeValidatedUTXO ->
    ValidatorUTXOs 'ForTransaction

-- | @since 1.0
deriving stock instance Show (ValidatorUTXOs p)

{- | An UTxO at a specified validator address. It will be used as 'Spending'
 in the 'ScriptPurpose' of the built 'ScriptContext'.

 @since 2.1
-}
data SomeValidatedUTXO where
  SomeValidatedUTXO ::
    forall (datum :: Type) (redeemer :: Type).
    (FromData datum, ToData datum, Show datum, Typeable datum,
     FromData redeemer, ToData redeemer, Show redeemer, Typeable redeemer) =>
    { someUTxO :: ValidatorUTXO datum
    , someSpendingScript :: TestScript ( 'ForSpending datum redeemer)
    , someRedeemer :: redeemer
    } ->
    SomeValidatedUTXO

instance Show SomeValidatedUTXO where
  show SomeValidatedUTXO {someUTxO, someRedeemer} =
    "SomeValidatedUTXO{someUTxO= " <> shows someUTxO (", someRedeemer= " <> shows someRedeemer "}")

-- | @since 1.0
instance Semigroup (ValidatorUTXOs p) where
  NoValidatorUTXOs <> x = x
  x <> NoValidatorUTXOs = x
  (ValidatorUTXOs m1) <> (ValidatorUTXOs m2) = ValidatorUTXOs $ m1 <> m2
  MultiValidatorUTXOs m1 <> MultiValidatorUTXOs m2 = MultiValidatorUTXOs $ m1 <> m2

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
newtype Minting
  = -- | @since 1.0
    Mint Value
  deriving stock
    ( -- | @since 1.0
      Eq, Show
    )
  deriving newtype
    ( -- | @since 2.1
      Semigroup, Monoid
    )

{- | Indicates whether a 'ContextBuilder' has named components or not.

 @since 2.0
-}
data Naming = Anonymous | Named
  deriving stock
    ( -- | @since 2.0
      Eq
    , -- | @since 2.0
      Show
    )

{- | A way to incrementally build up a 'ScriptContext'.

 This has two \'tags\':

 * A 'Purpose' to indicate what kind of 'ScriptPurpose' will be used in the
 resulting 'ScriptContext'; and
 * a 'Naming' to indicate whether named fragments are in use or not.

 You can use the 'Semigroup' instance of this type to build up larger
 contexts.

 @since 2.0
-}
data ContextBuilder (p :: Purpose) (n :: Naming) where
  NoNames :: ContextFragment p -> ContextBuilder p 'Anonymous
  WithNames :: Map Text (ContextFragment p) -> ContextBuilder p 'Named

-- | @since 2.0
deriving stock instance Show (ContextBuilder p n)

{- | Anonymous 'ContextBuilder's preserve all the information of each \'half\'
 when '<>'d. Named 'ContextBuilder's instead combine all the named fragments
 in each \'half\'.

 = Important note

 When combining named 'ContextBuilder's, if both \'halves\' have a fragment
 associated to a specific name, the fragment in the /second/ argument will
 everything in /both/ fragments.

 @since 2.0
-}
instance Semigroup (ContextBuilder p n) where
  {-# INLINEABLE (<>) #-}
  NoNames cf <> NoNames cf' = NoNames $ cf <> cf'
  WithNames cfs <> WithNames cfs' = WithNames . Map.unionWith (<>) cfs $ cfs'

-- | @since 2.0
instance Monoid (ContextBuilder p 'Anonymous) where
  {-# INLINEABLE mempty #-}
  mempty = NoNames mempty

-- | @since 2.0
instance Monoid (ContextBuilder p 'Named) where
  {-# INLINEABLE mempty #-}
  mempty = WithNames mempty

{- | Optionally-nameable piece of script context.

 @since 2.0
-}
data ContextFragment (p :: Purpose) = ContextFragment
  { -- | @since 2.0
    cfInputs :: Seq SideUTXO
  , -- | @since 2.0
    cfOutputs :: Seq SideUTXO
  , -- | @since 2.0
    cfSignatures :: Seq PubKeyHash
  , -- | @since 2.0
    cfDatums :: Seq BuiltinData
  , -- | @since 2.0
    cfMinting :: Seq Minting
  , -- | @since 2.0
    cfValidatorInputs :: ValidatorUTXOs p
  , -- | @since 2.0
    cfValidatorOutputs :: ValidatorUTXOs p
  }
  deriving stock
    ( -- | @since 2.0
      Show
    )

-- | @since 2.0
instance Semigroup (ContextFragment p) where
  {-# INLINEABLE (<>) #-}
  ContextFragment ins outs sigs ds ms vins vouts
    <> ContextFragment ins' outs' sigs' ds' ms' vins' vouts' =
      ContextFragment
        (ins <> ins')
        (outs <> outs')
        (sigs <> sigs')
        (ds <> ds')
        (ms <> ms')
        (vins <> vins')
        (vouts <> vouts')

-- | @since 2.0
instance Monoid (ContextFragment p) where
  {-# INLINEABLE mempty #-}
  mempty = ContextFragment mempty mempty mempty mempty mempty mempty mempty

{- | As 'spendingScriptContext', but with the default
transaction config 'defTransactionConfig'.

 @since 2.0
-}
spendingScriptContextDef ::
  forall (datum :: Type) (redeemer :: Type) (n :: Naming).
  (ToData datum) =>
  ContextBuilder ( 'ForSpending datum redeemer) n ->
  TestUTXO datum ->
  ScriptContext
spendingScriptContextDef = spendingScriptContext defTransactionConfig

{- | Construct 'ScriptContext' with 'Spending' 'ScriptPurpose' from the provided blocks.

 @since 2.0
-}
spendingScriptContext ::
  forall (datum :: Type) (redeemer :: Type) (n :: Naming).
  (ToData datum) =>
  TransactionConfig ->
  ContextBuilder ( 'ForSpending datum redeemer) n ->
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
  forall (r :: Type) (n :: Naming).
  ContextBuilder ( 'ForMinting r) n ->
  NonEmpty MintingPolicyTask ->
  ScriptContext
mintingScriptContextDef = mintingScriptContext defTransactionConfig

{- | Construct 'ScriptContext' with 'Minting' 'ScriptPurpose' from the provided blocks.

 @since 1.0
-}
mintingScriptContext ::
  forall (r :: Type) (n :: Naming).
  TransactionConfig ->
  ContextBuilder ( 'ForMinting r) n ->
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

transactionSpending ::
  (FromData d, ToData d, Show d) =>
  TestScript ( 'ForSpending d r) ->
  ValidatorUTXO d ->
  ContextBuilder 'ForTransaction n ->
  ContextBuilder ( 'ForSpending d r) n
transactionSpending script input (NoNames cf) =
  NoNames (transactionSpendingFragment script input cf)
transactionSpending script input (WithNames cfs) =
  WithNames (transactionSpendingFragment script input <$> cfs)

transactionSpendingFragment ::
  forall d r. (FromData d, ToData d, Show d) =>
  TestScript ( 'ForSpending d r) ->
  ValidatorUTXO d ->
  ContextFragment 'ForTransaction -> ContextFragment ( 'ForSpending d r)
transactionSpendingFragment
  spendingScript utxoToSpend
  ContextFragment{cfInputs, cfOutputs, cfSignatures, cfDatums, cfMinting, cfValidatorInputs, cfValidatorOutputs} =
  ContextFragment{cfInputs = cfInputs <> Seq.fromList (Map.elems otherValidatorInputs),
                  cfOutputs = cfOutputs <> Seq.fromList (Map.elems otherValidatorOutputs),
                  cfSignatures, cfDatums, cfMinting,
                  cfValidatorInputs = ValidatorUTXOs theValidatorInputs,
                  cfValidatorOutputs = ValidatorUTXOs theValidatorOutputs}
  where
    otherValidatorInputs, otherValidatorOutputs :: Map Text SideUTXO
    (otherValidatorInputs, theValidatorInputs) = Map.mapEither transactionSpendingUTxO (getUTxOs cfValidatorInputs)
    (otherValidatorOutputs, theValidatorOutputs) = Map.mapEither transactionSpendingUTxO (getUTxOs cfValidatorOutputs)
    transactionSpendingUTxO :: SomeValidatedUTXO -> Either SideUTXO (ValidatorUTXO d)
    transactionSpendingUTxO SomeValidatedUTXO{someUTxO, someSpendingScript}
      | encoded someUTxO == encoded utxoToSpend && getTestValidator someSpendingScript == getTestValidator spendingScript = Right utxoToSpend
      | otherwise = Left SideUTXO{sUtxoType = ScriptUTXO (validatorHash $ getTestValidator someSpendingScript) $ toBuiltinData $ vUtxoDatum someUTxO,
                                  sUtxoValue = GeneralValue $ vUtxoValue someUTxO}
    encoded :: forall datum. ToData datum => ValidatorUTXO datum -> ValidatorUTXO BuiltinData
    encoded (ValidatorUTXO d v) = ValidatorUTXO (toBuiltinData d) v

transactionMinting ::
  TestScript ( 'ForMinting r) ->
  ContextBuilder 'ForTransaction n ->
  ContextBuilder ( 'ForMinting r) n
transactionMinting script (NoNames cf) =
  NoNames (transactionMintingFragment script cf)
transactionMinting script (WithNames cfs) =
  WithNames (transactionMintingFragment script <$> cfs)

transactionMintingFragment ::
  TestScript ( 'ForMinting r) ->
  ContextFragment 'ForTransaction -> ContextFragment ( 'ForMinting r)
transactionMintingFragment
  mintingPolicy
  ContextFragment{cfInputs, cfOutputs, cfSignatures, cfDatums, cfMinting, cfValidatorInputs, cfValidatorOutputs} =
  ContextFragment{cfInputs = cfInputs <> Seq.fromList (Map.elems otherValidatorInputs),
                  cfOutputs = cfOutputs <> Seq.fromList (Map.elems otherValidatorOutputs),
                  cfSignatures, cfDatums,
                  cfMinting = Seq.filter (/= mempty) (otherMint <$> cfMinting),
                  cfValidatorInputs = mempty,
                  cfValidatorOutputs = mempty}
  where
    otherMint :: Minting -> Minting
    otherMint (Mint val) = Mint (filterValue otherSymbol val)
    otherSymbol symbol _ _ = symbol /= Value.mpsSymbol (mintingPolicyHash $ getTestMintingPolicy mintingPolicy)
    otherValidatorInputs, otherValidatorOutputs :: Map Text SideUTXO
    otherValidatorInputs =  transactionUTxO <$> getUTxOs cfValidatorInputs
    otherValidatorOutputs = transactionUTxO <$> getUTxOs cfValidatorOutputs
    transactionUTxO :: SomeValidatedUTXO -> SideUTXO
    transactionUTxO SomeValidatedUTXO{someUTxO, someSpendingScript} =
      SideUTXO{sUtxoType = ScriptUTXO (validatorHash $ getTestValidator someSpendingScript) $ toBuiltinData $ vUtxoDatum someUTxO,
               sUtxoValue = GeneralValue $ vUtxoValue someUTxO}

getUTxOs :: ValidatorUTXOs 'ForTransaction -> Map.Map Text SomeValidatedUTXO
getUTxOs NoValidatorUTXOs = mempty
getUTxOs (MultiValidatorUTXOs m) = m

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
  forall (p :: Purpose) (n :: Naming).
  TransactionConfig ->
  ContextBuilder p n ->
  TxInfo
baseTxInfo conf = go . foldBuilt
  where
    go :: ContextFragment p -> TxInfo
    go cf =
      let value = foldMap toUsefulValue . cfMinting $ cf
          insList = toList . Seq.filter (checkSideUtxoAddress conf) . cfInputs $ cf
          outsList = toList . Seq.filter (checkSideUtxoAddress conf) . cfOutputs $ cf
          vins = cfValidatorInputs cf
          vouts = cfValidatorOutputs cf
          pkhs = toList . cfSignatures $ cf
          dats = toList . fmap datumWithHash . cfDatums $ cf
       in starterTxInfo
            { txInfoMint = value
            , txInfoInputs = createTxInInfos conf insList vins
            , txInfoOutputs = createTxOuts conf outsList vouts
            , txInfoSignatories = pkhs
            , txInfoData =
                validatorUtxosToDatum vins
                  <> mapMaybe sideUtxoToDatum insList
                  <> mapMaybe sideUtxoToDatum outsList
                  <> dats
            }
    toUsefulValue :: Minting -> Value
    toUsefulValue (Mint val) = filterValue filterCS val
    filterCS :: CurrencySymbol -> TokenName -> Integer -> Bool
    filterCS cs _ _ = cs /= testCurrencySymbol conf
    starterTxInfo :: TxInfo
    starterTxInfo =
      TxInfo
        { txInfoInputs = []
        , txInfoOutputs = []
        , txInfoFee = testFee conf
        , txInfoMint = mempty
        , txInfoDCert = []
        , txInfoWdrl = []
        , txInfoValidRange = testTimeRange conf
        , txInfoSignatories = []
        , txInfoData = []
        , txInfoId = TxId "testTx"
        }

{- | Turns an arbitrary 'ContextBuilder' into a 'ContextFragment'.

= Note

 This is a low-level operation designed for maximum control. If possible, use
 the other, higher-level, operations in this module instead.

   @since 2.1
-}
foldBuilt :: ContextBuilder p n -> ContextFragment p
foldBuilt = \case
  NoNames cf -> cf
  WithNames cfs -> fold cfs

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
  MultiValidatorUTXOs m ->
    map
      ( \SomeValidatedUTXO {someUTxO = ValidatorUTXO dt _} ->
          datumWithHash . toBuiltinData $ dt
      )
      $ Map.elems m

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

someValidatedUtxoToTxOut ::
  SomeValidatedUTXO ->
  TxOut
someValidatedUtxoToTxOut
  SomeValidatedUTXO {someUTxO = ValidatorUTXO dat val, someSpendingScript = validator} =
    TxOut
      { txOutAddress = scriptAddress $ getTestValidator validator
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
        MultiValidatorUTXOs m -> fmap someValidatedUtxoToTxOut $ Map.elems m
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
